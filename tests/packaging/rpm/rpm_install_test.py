from tests import *
import pytest
import tests.utils.path_utils
import tests.utils.utils
import tests.packaging.oneprovider_common

from environment import docker, env

file_dir = os.path.dirname(os.path.realpath(__file__))

with open('./RELEASE', 'r') as f:
    release = f.read().replace('\n', '')

class Distribution(object):

    def __init__(self, request, link={}, privileged=False):
        package_dir = os.path.join(os.getcwd(), 'package/{0}/x86_64'.
                                   format(request.param))
        config_dir = os.path.join(file_dir, 'rpm_install_test_data')

        self.name = request.param
        self.release = release
        self.image = {
            'centos-7-x86_64': 'centos:7'
        }[self.name]
        self.repo = {
            'centos-7-x86_64': 'centos_7x'
        }[self.name]
        self.container = docker.run(interactive=True,
                                    tty=True,
                                    detach=True,
                                    image=self.image,
                                    hostname='onedata.test.local',
                                    privileged=True,
                                    link=link,
                                    stdin=sys.stdin,
                                    stdout=sys.stdout,
                                    stderr=sys.stderr,
                                    volumes=[
                                        (package_dir, '/root/pkg', 'ro'),
                                        (config_dir, '/root/data', 'ro')
                                    ],
                                    reflect=[('/sys/fs/cgroup', 'rw')])

        request.addfinalizer(lambda: docker.remove(
            [self.container], force=True, volumes=True))


@pytest.fixture(scope='module')
def setup_command():
    return 'yum -y update ; yum clean all && yum -y update && ' \
        'yum -y install ca-certificates python wget curl && ' \
        'yum -y install epel-release || true && ' \
        'curl -sSL "{url}/yum/{release}/onedata_{{repo}}.repo" > /etc/yum.repos.d/onedata.repo' \
        .format(url='http://packages.onedata.org', release=release)


@pytest.fixture(scope='module')
def onezone(request):
    class Onezone(object):

        def __init__(self, nodes):
            (_, _, domain) = nodes[0].partition('@')
            self.domain = domain

    result = env.up(tests.utils.path_utils.config_file('env.json'),
                    image='onedata/worker:1802-1')

    request.addfinalizer(lambda: docker.remove(
        result['docker_ids'], force=True, volumes=True))

    return Onezone(result['oz_worker_nodes'])


@pytest.fixture(scope='module',
                params=['centos-7-x86_64'])
def oneclient(request, setup_command):
    distribution = Distribution(request)
    command = setup_command.format(repo=distribution.repo, release=distribution.release)

    assert 0 == docker.exec_(distribution.container,
                             interactive=True,
                             tty=True,
                             command=command)

    return distribution


@pytest.fixture(scope='module',
                params=['centos-7-x86_64'])
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
    command = setup_command.format(repo=distribution.repo,
                                   release=distribution.release)
    command = '{command} && ' \
        'yum -y install python-setuptools python-pip && ' \
        'pip install --upgrade pip && ' \
        'pip install requests'.format(command=command)

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


def test_oneclient_base_installation(oneclient):
    assert 0 == docker.exec_(oneclient.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_oneclient_base.py {}'.format(release,))


def test_fsonedatafs_installation(oneclient):
    assert 0 == docker.exec_(oneclient.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_fsonedatafs.py')


def test_oneprovider_installation(oneprovider):
    result = docker.exec_(oneprovider.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_oneprovider.py {}'.format(release,))
    config_file = tests.utils.path_utils.config_file('config.yml')
    tests.packaging.oneprovider_common.reset_oz_domain_in_config(config_file)
    assert 0 == result
