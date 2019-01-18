from tests import *
import pytest
import tests.utils.path_utils
import tests.utils.utils
import tests.packaging.oneprovider_common

from environment import docker, env

file_dir = os.path.dirname(os.path.realpath(__file__))


class Distribution(object):

    def __init__(self, request, link={}, privileged=False):
        package_dir = os.path.join(os.getcwd(), 'package/{0}/binary-amd64'.
                                   format(request.param))
        config_dir = os.path.join(file_dir, 'deb_install_test_data')

        self.name = request.param
        self.image = 'ubuntu:{0}'.format(self.name)
        self.container = docker.run(interactive=True,
                                    tty=True,
                                    detach=True,
                                    image=self.image,
                                    hostname='onedata.test.local',
                                    privileged=privileged,
                                    link=link,
                                    stdin=sys.stdin,
                                    stdout=sys.stdout,
                                    stderr=sys.stderr,
                                    volumes=[
                                        (package_dir, '/root/pkg', 'ro'),
                                        (config_dir, '/root/data', 'ro')
                                    ])

        request.addfinalizer(lambda: docker.remove(
            [self.container], force=True, volumes=True))


@pytest.fixture(scope='module')
def setup_command():
    return 'apt-get update && ' \
        'apt-get install -y ca-certificates locales python wget && ' \
        'wget -qO- {url}/onedata.gpg.key | apt-key add - && ' \
        'echo "deb {url}/apt/ubuntu/{{dist}} {{dist}} main" > /etc/apt/sources.list.d/onedata.list && ' \
        'echo "deb-src {url}/apt/ubuntu/{{dist}} {{dist}} main" >> /etc/apt/sources.list.d/onedata.list && ' \
        'apt-get update && ' \
        'locale-gen en_US.UTF-8'.format(url='http://packages.onedata.org')


@pytest.fixture(scope='module')
def onezone(request):
    class Onezone(object):

        def __init__(self, nodes):
            (_, _, domain) = nodes[0].partition('@')
            self.domain = domain

    result = env.up(tests.utils.path_utils.config_file('env.json'),
                    image='onedata/worker:v61')

    request.addfinalizer(lambda: docker.remove(
        result['docker_ids'], force=True, volumes=True))

    return Onezone(result['oz_worker_nodes'])


@pytest.fixture(scope='module',
                params=['trusty', 'xenial'])
def oneclient(request, setup_command):
    distribution = Distribution(request, privileged=True)
    command = setup_command.format(dist=distribution.name)

    assert 0 == docker.exec_(distribution.container,
                             interactive=True,
                             tty=True,
                             command=command)

    return distribution


@pytest.fixture(scope='module',
                params=['xenial'])
def oneclient_base(request, setup_command):
    distribution = Distribution(request, privileged=True)
    command = setup_command.format(dist=distribution.name)

    assert 0 == docker.exec_(distribution.container,
                             interactive=True,
                             tty=True,
                             command=command)

    return distribution


@pytest.fixture(scope='module',
                params=['xenial'])
def oneprovider(request, onezone, setup_command):
    onezone_node = onezone.domain
    # onezone_node is in format node.oz.1234.test, resolve domain (oz.1234.test)
    onezone_domain = tests.utils.utils.get_domain(onezone_node)
    # Put the domain in config so the provider knows where to register
    config_file = tests.utils.path_utils.config_file('config.yml')
    tests.packaging.oneprovider_common.update_oz_domain_in_config(
        config_file, onezone_domain)
    # Link provider docker to the OZ node (this way we do not need DNS here).
    # This link will cause connections to 'oz.1234.test' reach 'node.oz.1234.test'
    distribution = Distribution(request, link={onezone_node: onezone_domain})
    command = setup_command.format(dist=distribution.name)
    command = '{command} && ' \
        'apt-get install -y python-setuptools && ' \
        'easy_install requests'.format(command=command)

    assert 0 == docker.exec_(distribution.container,
                             interactive=True,
                             tty=True,
                             command=command)
    return distribution


def test_oneclient_installation(oneclient):
    assert 0 == docker.exec_(oneclient.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_oneclient.py')


def test_oneclient_base_installation(oneclient_base):
    assert 0 == docker.exec_(oneclient_base.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_oneclient_base.py')


def test_oneprovider_installation(oneprovider):
    result = docker.exec_(oneprovider.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_oneprovider.py')
    config_file = tests.utils.path_utils.config_file('config.yml')
    tests.packaging.oneprovider_common.reset_oz_domain_in_config(config_file)
    assert 0 == result
