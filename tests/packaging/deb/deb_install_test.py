from tests import *
import pytest
import json
import tests.utils.path_utils
import tests.utils.utils
import tests.packaging.oneprovider_common

from environment import docker, env

file_dir = os.path.dirname(os.path.realpath(__file__))

with open('./RELEASE', 'r') as f:
    release = f.read().replace('\n', '')

class Distribution(object):

    def __init__(self, request, link={}, privileged=False):
        package_dir = os.path.join(os.getcwd(), 'package/{0}/binary-amd64'.
                                   format(request.param))
        config_dir = os.path.join(file_dir, 'deb_install_test_data')

        self.name = request.param
        self.release = release
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
    return 'echo -n \'Acquire::http::Proxy \"http://proxy.devel.onedata.org:3128\";\' > /etc/apt/apt.conf.d/proxy.conf && ' \
        'apt-get update && ' \
        'apt-get install -y ca-certificates locales python wget curl gnupg && ' \
        'wget -qO- {url}/onedata.gpg.key | apt-key add - && ' \
        'echo "deb {url}/apt/ubuntu/{{release}} {{dist}} main" > /etc/apt/sources.list.d/onedata.list && ' \
        'echo "deb-src {url}/apt/ubuntu/{{release}} {{dist}} main" >> /etc/apt/sources.list.d/onedata.list && ' \
        'apt-get update && ' \
        'locale-gen en_US.UTF-8'.format(url='http://packages.onedata.org')


@pytest.fixture(scope='module')
def onezone(request):
    config_path = tests.utils.path_utils.config_file('env.json')

    class Onezone(object):

        def __init__(self, nodes, dockers):
            (_, _, domain) = nodes[0].partition('@')
            self.domain = domain
            self.node = "worker@" + domain
            self.cookie = tests.utils.utils.get_oz_cookie(
                config_path, self.node)
            self.dockers = dockers

    result = env.up(tests.utils.path_utils.config_file('env.json'),
                    image='onedata/worker:{}-2'.format(release,))
    dockers = result['docker_ids']

    request.addfinalizer(lambda: docker.remove(
        dockers, force=True, volumes=True))

    return Onezone(result['oz_worker_nodes'], dockers)


@pytest.fixture(scope='module',
                params=['xenial', 'bionic', 'focal'])
def oneclient(request, setup_command):
    distribution = Distribution(request, privileged=True)
    command = setup_command.format(dist=distribution.name,
                                   release=distribution.release)

    assert 0 == docker.exec_(distribution.container,
                             interactive=True,
                             tty=True,
                             command=command)

    return distribution


@pytest.fixture(scope='module',
                params=['bionic'])
def oneclient_base(request, setup_command):
    distribution = Distribution(request, privileged=True)
    command = setup_command.format(dist=distribution.name,
                                   release=distribution.release)

    assert 0 == docker.exec_(distribution.container,
                             interactive=True,
                             tty=True,
                             command=command)

    return distribution


@pytest.fixture(scope='module',
                params=['bionic'])
def oneprovider(request, onezone, setup_command):
    onezone_node = onezone.domain
    # onezone_node is in format node.oz.1234.test, resolve domain (oz.1234.test)
    onezone_domain = tests.utils.utils.get_domain(onezone_node)
    # Put the domain in config so the provider knows where to register
    config_file = tests.utils.path_utils.config_file('config.yml')

    # Link provider docker to the OZ node (this way we do not need DNS here).
    # This link will cause connections to 'oz.1234.test' reach 'node.oz.1234.test'
    distribution = Distribution(request, link={onezone_node: onezone_domain})
    command = setup_command.format(dist=distribution.name,
                                   release=distribution.release)
    command = '{command} && ' \
        'apt-get install -y python-pip gnupg2 libssl1.0.0 && ' \
        'pip install requests'.format(command=command)

    assert 0 == docker.exec_(distribution.container,
                             interactive=True,
                             tty=True,
                             command=command)

    registration_token = get_registration_token(distribution, onezone_domain)
    tests.packaging.oneprovider_common.update_token_in_config(
        config_file, registration_token)

    return distribution


def get_registration_token(distribution, onezone_domain):
    uri = '/api/v3/onezone/user/clusters/provider_registration_token/'
    cmd = 'curl -Ss -k -X POST -u provideradmin:password https://{}{}'.format(
        onezone_domain, uri)
    output = docker.exec_(distribution.container, interactive=True,
                          tty=True, output=True, command=cmd).strip()
    return json.loads(output)['token']

def test_oneclient_base_installation(oneclient_base):
    assert 0 == docker.exec_(oneclient_base.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_oneclient_base.py {}'
                                     .format(oneclient_base.name))

def test_fsonedatafs_installation(oneclient_base):
    assert 0 == docker.exec_(oneclient_base.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_fsonedatafs.py {}'
                                     .format(oneclient_base.name))

def test_oneclient_installation(oneclient):
    assert 0 == docker.exec_(oneclient.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_oneclient.py {}'
                                      .format(oneclient.name))


def test_oneprovider_installation(oneprovider):
    result = docker.exec_(oneprovider.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_oneprovider.py {}'
                                     .format(oneprovider.name))
    config_file = tests.utils.path_utils.config_file('config.yml')
    tests.packaging.oneprovider_common.reset_token_in_config(config_file)
    assert 0 == result
