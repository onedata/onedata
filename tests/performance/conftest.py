"""
This module contains functions that are used to run performance tests on
acceptance level.
"""
import inspect
import json

from environment import docker

from tests import *
from tests.utils.client_utils import mount_users
from tests.utils.file_utils import get_file_name
from tests.utils.git_utils import get_branch_name, get_commit, get_repository
from tests.utils.performance_utils import *
from tests.utils.utils import get_copyright, get_authors, get_suite_description

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

    # @pytest.fixture(scope="module", params=get_json_files(PERFORMANCE_ENV_DIR))
    # def env_description_file(self, request):
    #     """This fixture must be overridden in performance test module if you
    #     want to start tests from given module with different environments that
    #     those defined in performance/environments directory
    #     """
    #     return request.param

    # @pytest.fixture(scope="module")
    # def persistent_environment(self, request, env_description_file):
    #     test_name = get_file_name(inspect.getfile(self.__class__))
    #     logdir = make_logdir(PERFORMANCE_LOGDIR, test_name)
    #     env = run_env_up_script("env_up.py",
    #                             logdir=logdir,
    #                             config=env_description_file)
    #
    #     def fin():
    #         docker.remove(env['docker_ids'], force=True, volumes=True)
    #
    #     request.addfinalizer(fin)
    #     return env

    # @pytest.fixture()
    # def environment(self, persistent_environment, request, env_description_file):
    #
    #     def fin():
    #         if 'posix' in persistent_environment['storages'].keys():
    #             for storage_name, storage in persistent_environment['storages']['posix'].items():
    #                 clear_storage(storage['host_path'])
    #
    #     request.addfinalizer(fin)
    #     return persistent_environment
    #

    # TODO this fixture is similiar to client_ids in cucumber tests
    # TODO it should be moved to common conftest.py
    @pytest.fixture()
    def clients(self, request, environment, context, client_ids,
                env_description_file):
        # users = self.users.keys()
        # clients = [users[u] for u in users]
        # print "CLIENTS: "

        print get_users(environment)
        # assert False

        mount_users(request, environment, context, client_ids,
                    env_description_file, **get_users(environment))


def get_users(environment):
    data = environment['client_data']
    users = []
    client_instances = []
    mount_paths = []
    client_hosts = []
    tokens = []
    for client_host in data.keys():
        for client_instance in data[client_host].keys():
            d = data[client_host][client_instance]
            users.append(d['token_for'])
            client_instances.append(client_instance)
            mount_paths.append(d['mounting_path'])
            client_hosts.append(client_host)
            tokens.append('token')
    return {"users": users,
            "client_instances": client_instances,
            "mount_paths": mount_paths,
            "client_hosts": client_hosts,
            "tokens": tokens}
