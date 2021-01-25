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
    return 'echo "proxy=http://proxy.devel.onedata.org:3128" >> /etc/yum.conf && ' \
        'sed -i "s/enabled=1/enabled=0/" /etc/yum/pluginconf.d/fastestmirror.conf && ' \
        'sed -i "s/mirrorlist/#mirrorlist/" /etc/yum.repos.d/CentOS-Base.repo && ' \
        'sed -i "s/#baseurl/baseurl/" /etc/yum.repos.d/CentOS-Base.repo && ' \
        'yum -y update ; yum clean all && yum -y update && ' \
        'yum -y install ca-certificates python wget curl && ' \
        'yum -y install epel-release || true && ' \
        'curl -sSL "{url}/yum/{release}/onedata_{{repo}}.repo" > /etc/yum.repos.d/onedata.repo' \
        .format(url='http://packages.onedata.org', release=release)


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

    # Link provider docker to the OZ node (this way we do not need DNS here).
    # This link will cause connections to 'oz.1234.test' reach 'node.oz.1234.test'
    distribution = Distribution(request, link={onezone_node: onezone_domain})
    command = setup_command.format(repo=distribution.repo,
                                   release=distribution.release)
    command = '{command} && ' \
        'yum -y install python-setuptools python-pip && ' \
        'pip install --upgrade pip==20.3.4 && ' \
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
    tests.packaging.oneprovider_common.reset_token_in_config(config_file)
    assert 0 == result
