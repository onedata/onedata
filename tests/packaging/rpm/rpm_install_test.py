from tests import *
import pytest
import tests.utils.path_utils

from environment import docker, env

file_dir = os.path.dirname(os.path.realpath(__file__))


class Distribution(object):

    def __init__(self, request, link={}, privileged=False):
        package_dir = os.path.join(os.getcwd(), 'package/{0}/x86_64'.
                                   format(request.param))
        config_dir = os.path.join(file_dir, 'rpm_install_test_data')

        self.name = request.param
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
                                    hostname='onedata.dev.local',
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
        'yum -y install ca-certificates python wget && ' \
        'yum -y install epel-release || true && ' \
        'wget -qO- "{url}/yum/onedata_{{repo}}.repo" > /etc/yum.repos.d/onedata.repo' \
        .format(url='http://packages.onedata.org')


@pytest.fixture(scope='module')
def onezone(request):
    class Onezone(object):

        def __init__(self, nodes):
            (_, _, domain) = nodes[0].partition('@')
            self.domain = domain

    result = env.up(tests.utils.path_utils.config_file('env.json'))

    request.addfinalizer(lambda: docker.remove(
        result['docker_ids'], force=True, volumes=True))

    return Onezone(result['oz_worker_nodes'])


@pytest.fixture(scope='module',
                params=['centos-7-x86_64'])
def oneclient(request, setup_command):
    distribution = Distribution(request)
    command = setup_command.format(repo=distribution.repo)

    assert 0 == docker.exec_(distribution.container,
                             interactive=True,
                             tty=True,
                             command=command)

    return distribution


@pytest.fixture(scope='module',
                params=['centos-7-x86_64'])
def oneprovider(request, onezone, setup_command):
    distribution = Distribution(request, link={onezone.domain: 'onedata.org'})
    command = setup_command.format(repo=distribution.repo)
    command = '{command} && ' \
        'yum -y install python-setuptools && ' \
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


def test_oneprovider_installation(oneprovider):
    assert 0 == docker.exec_(oneprovider.container,
                             interactive=True,
                             tty=True,
                             command='python /root/data/install_oneprovider.py')
