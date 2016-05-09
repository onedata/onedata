"""
This module contains functions that are used to run performance tests on
acceptance level.
"""
import inspect
import json

from environment import docker

from tests import *
from tests.utils.client_utils import Client
from tests.cucumber.scenarios.steps.env_steps import clear_storage
from tests.cucumber.scenarios.steps.multi_auth_steps import (set_dns, get_cookie,
                                                       get_token)
from tests.utils.file_utils import get_file_name, make_logdir, get_json_files
from tests.utils.git_utils import get_branch_name, get_commit, get_repository
from tests.utils.performance_utils import *
from tests.utils.utils import run_env_up_script, get_copyright, get_authors, \
    get_suite_description, set_dns, get_token, get_cookie

__author__ = "Jakub Kudzia"
__copyright__ = """(C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'."""


@pytest.fixture(scope="session")
def json_output(request):
    performance_report = PerformanceReport("performance", get_repository(),
                                           get_commit(), get_branch_name())

    def fin():
        if not os.path.exists(PERFORMANCE_LOGDIR):
            os.makedirs(PERFORMANCE_LOGDIR)
        f = open(PERFORMANCE_OUTPUT, 'w')
        f.write(json.dumps(performance_report.report))

    request.addfinalizer(fin)
    return performance_report


class AbstractPerformanceTest:
    @pytest.fixture(scope="module")
    def suite_report(self, request, env_report):
        module = inspect.getmodule(self.__class__)
        name = get_file_name(inspect.getfile(self.__class__))
        report = SuiteReport(name, get_suite_description(module),
                             get_copyright(module), get_authors(module))

        def fin():
            env_report.add_to_report("suites", report)

        request.addfinalizer(fin)
        return report

    @pytest.fixture(scope="module")
    def env_report(self, request, json_output, env_description_file):
        name = env_description_file.split(os.path.sep)[-1]
        report = EnvironmentReport(name)

        def fin():
            json_output.add_to_report("envs", report)

        request.addfinalizer(fin)
        return report

    @pytest.fixture(scope="module", params=get_json_files(PERFORMANCE_ENV_DIR))
    def env_description_file(self, request):
        """This fixture must be overridden in performance test module if you
        want to start tests from given module with different environments that
        those defined in performance/environments directory
        """
        return request.param

    @pytest.fixture(scope="module")
    def persistent_environment(self, request, env_description_file):
        test_name = get_file_name(inspect.getfile(self.__class__))
        logdir = make_logdir(PERFORMANCE_LOGDIR, test_name)
        env = run_env_up_script("env_up.py",
                                logdir=logdir,
                                config=env_description_file)

        def fin():
            docker.remove(env['docker_ids'], force=True, volumes=True)

        request.addfinalizer(fin)
        return env

    @pytest.fixture()
    def environment(self, persistent_environment, request, env_description_file):

        def fin():
            if 'posix' in persistent_environment['storages'].keys():
                for storage_name, storage in persistent_environment['storages']['posix'].items():
                    clear_storage(storage['host_path'])

        request.addfinalizer(fin)
        return persistent_environment


    # TODO this fixture is similiar to client_ids in cucumber tests
    # TODO it should be moved to common conftest.py
    @pytest.fixture()
    def clients(self, environment, env_description_file):
        client_dockers = environment['client_nodes']
        # current version is for environment withenvironment one OZ
        oz_node = environment['oz_worker_nodes'][0]
        set_dns(environment)
        client_data = environment['client_data']
        mounted_clients = {}
        for client_docker in client_dockers:
            client_host, _sep, _timestamp = client_docker.partition('.')
            for client in client_data[client_host].keys():
                cookie = get_cookie(env_description_file, oz_node)
                token = get_token(client_data[client_host][client]['token_for'],
                                  oz_node, cookie)

                token_path = "/tmp/token"
                client_name = client_data[client_host][client]['client_name']
                mount_path = client_data[client_host][client]['mounting_path']
                mounted_clients[client_name] = Client(client_docker, mount_path)
                data = client_data[client_host][client]
                # /root has to be accessible for gdb to access /root/bin/oneclient
                assert run_cmd('root', mounted_clients[client_name],
                               'chmod +x /root') == 0
                cmd = ('mkdir -p {mount_path}'
                       ' && export GLOBAL_REGISTRY_URL={gr_domain}'
                       ' && export PROVIDER_HOSTNAME={op_domain}'
                       ' && export X509_USER_CERT={user_cert}'
                       ' && export X509_USER_KEY={user_key}'
                       ' && echo {token} > {token_path}'
                       ' && gdb oneclient -batch -return-child-result -ex \'run --authentication token --no_check_certificate {mount_path} < {token_path}\' -ex \'bt\' 2>&1'
                       ' && rm {token_path}').format(
                        mount_path=mount_path,
                        gr_domain=data['zone_domain'],
                        op_domain=data['op_domain'],
                        user_cert=data['user_cert'],
                        user_key=data['user_key'],
                        token=token,
                        token_path=token_path)

                user = client_data[client_host][client]['token_for']
                mounted_clients[client_name].user = user
                run_cmd(user, mounted_clients[client_name], cmd, output=True)
        return mounted_clients
