"""
Definitions of fixtures used in acceptance, cucumber and performance tests.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from tests.utils.path_utils import make_logdir, get_file_name, get_json_files
from tests.utils.utils import run_env_up_script, hostname, get_domain

from environment import docker

import pytest
import os
import tempfile
import shutil


def pytest_addoption(parser):
    parser.addoption("--test-type", action="store", default=None,
                     help="type of test (cucumber, acceptance,"
                          "performance, packaging)")


def pytest_generate_tests(metafunc):
    if 'test_type' in metafunc.fixturenames:
        test_type = metafunc.config.option.test_type
        if test_type in ['cucumber', 'performance']:
            envs = get_json_files(map_test_type_to_env_dir(test_type),
                                  relative=True)
            metafunc.parametrize(
                    ("test_type", 'env'),
                    [(test_type, env) for env in envs],
                    scope='module')


@pytest.fixture(scope="module")
def test_type():
    pass


@pytest.fixture(scope="module")
def env_description_file(request, test_type, env):
    """NOTE: If you want to start tests in given suite with environments
    different than all .json files from DEFAULT_CUCUMBER_ENV_DIR or
    PERFORMANCE_ENV_DIR (cucumber and performance tests respectively)
    this fixture must be overridden in that test module. As params
    for overridden fixture you must specify .json files with description
    of test environment for which you want tests to be started.
    This fixture must return absolute path to given .json.
    """
    absolute_path = os.path.join(map_test_type_to_env_dir(test_type), env)
    return absolute_path


@pytest.fixture(scope="module")
def persistent_environment(request, test_type, env_description_file):
    """
    Sets up environment and returns environment description.
    """
    curr_path = os.path.dirname(os.path.abspath(__file__))
    env_path = os.path.join(curr_path, '..', '..', 'environments',
                            env_description_file)

    logdir_path = map_test_type_to_logdir(test_type)

    feature_name = request.module.__name__.split('.')[-1]
    logdir = make_logdir(logdir_path, os.path
                         .join(get_file_name(env_description_file), feature_name))
    env_desc = run_env_up_script("env_up.py", config=env_path, logdir=logdir)

    def fin():
        docker.remove(request.environment['docker_ids'],
                      force=True,
                      volumes=True)

    request.addfinalizer(fin)
    request.environment = env_desc
    return env_desc


@pytest.fixture()
def environment(persistent_environment, request):

    def fin():
        if 'posix' in persistent_environment['storages'].keys():
            for storage_name, storage in persistent_environment['storages']['posix'].items():
                clear_storage(storage['host_path'])

    request.addfinalizer(fin)
    return persistent_environment


@pytest.fixture(scope="module")
def client_dockers(persistent_environment, context):
    ids = {}
    for client in persistent_environment['client_nodes']:
        client = str(client)
        client_name = client.split(".")[0]
        ids[client_name] = client
    return ids


@pytest.fixture(scope="module")
def providers(persistent_environment, request):
    op_worker_nodes = persistent_environment['op_worker_nodes']
    # current version is for one OZ
    oz_domain = persistent_environment['oz_worker_nodes'][0].split(".", 1)[-1]
    providers = {}
    for op_worker_node in op_worker_nodes:
        op_hostname = hostname(op_worker_node)
        op_domain = get_domain(op_hostname)
        provider_id = op_domain.split('.')[0]
        if provider_id not in providers.keys():
            new_provider = Provider(provider_id, op_domain, oz_domain)
            new_provider.copy_certs_from_docker(op_hostname)
            providers[provider_id] = new_provider

    def fin():
        for provider in providers.itervalues():
            provider.delete_certs()

    request.addfinalizer(fin)
    return providers


@pytest.fixture()
def skip_by_env(request, env_description_file):
    """This function skips test cases decorated with:
    @pytest.mark.skip_env(*envs).
    Test won't start for each env in envs.
    If you want to skip whole module, you must define
    global variable in that module named pytestmark in
    the following way:
    pytestmark = pytest.mark.skip_env(*envs)
    """
    if request.node.get_marker('skip_env'):
        env = get_file_name(env_description_file)
        args = request.node.get_marker('skip_env').kwargs
        reason = args['reason']
        arg_envs = [get_file_name(e) for e in args['envs']]
        if env in arg_envs:
                pytest.skip('skipped on env: {env} with reason: {reason}'
                            .format(env=env, reason=reason))


@pytest.fixture()
def xfail_by_env(request, env_description_file):
    """This function marks test cases decorated with:
    @pytest.mark.skip_env(*envs)
    as expected to fail:
    Test will be marked as expected to fail for each
    env in envs.
    If you want to mark whole module, you must define
    global variable in that module named pytestmark in
    the following way:
    pytestmark = pytest.mark.xfail_env(*envs)
    Running tests with --runxfail causes tests marked as xfail to run
    """
    if request.node.get_marker('xfail_env'):
        env = get_file_name(env_description_file)
        args = request.node.get_marker('xfail_env').kwargs
        reason = args['reason']
        arg_envs = [get_file_name(e) for e in args['envs']]
        if env in arg_envs:
            pytest.xfail('xfailed on env: {env} with reason: {reason}'
                         .format(env=env, reason=reason))


def map_test_type_to_env_dir(test_type):
    return {
        'cucumber': DEFAULT_CUCUMBER_ENV_DIR,
        'performance': PERFORMANCE_ENV_DIR
    }[test_type]


def map_test_type_to_logdir(test_type):
    return {
        'cucumber': CUCUMBER_LOGDIR,
        'performance': PERFORMANCE_LOGDIR
    }.get(test_type, CUCUMBER_LOGDIR)


def clear_storage(storage_path):
    # we don't have permissions to clean storage directory
    # therefore docker with this directory mounted is started
    # (docker has root permissions) and dir is cleaned via docker
    cmd = 'sh -c "rm -rf {path}"'.format(path=os.path.join(storage_path, '*'))
    docker.run(tty=True,
               rm=True,
               interactive=True,
               reflect=[(storage_path, 'rw')],
               image='onedata/worker',
               command=cmd)


class Context:
    def __init__(self):
        self.users = {}

    def get_user(self, user):
        return self.users.get(user)

    def get_users(self, user_names):
        return [self.get_user(user_name) for user_name in user_names]

    def has_user(self, user_name):
        return user_name in self.users.keys()

    def get_client(self, user, client_node):
        return self.users[user].get_client(client_node)

    def add_user(self, user):
        self.users[user.name] = user


@pytest.fixture(scope="module")
def context(env_description_file):
    return Context()


class Provider:
    def __init__(self, id, domain, oz_domain):
        self.id = id
        self.domain = domain
        self.cert_dir = tempfile.mkdtemp()
        self.key_file = os.path.join(self.cert_dir, PROVIDER_KEY_FILE)
        self.cert_file = os.path.join(self.cert_dir, PROVIDER_CERT_FILE)
        self.spaces = {}
        self.oz_domain = oz_domain

    def copy_certs_from_docker(self, op_hostname):
        docker.cp(op_hostname, PROVIDER_KEY_PATH, self.cert_dir, False)
        docker.cp(op_hostname, PROVIDER_CERT_PATH, self.cert_dir, False)

    def delete_certs(self):
        shutil.rmtree(self.cert_dir)
