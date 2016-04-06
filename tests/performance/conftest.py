"""
Author: Jakub Kudzia
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements fixtures used in performance tests.
"""

import os
import sys
from utils import *

# these commands add 'tests' to path to make it possible
# to import 'test_common'
curr_dir = os.path.dirname(os.path.realpath(__file__))
curr_dir_list = curr_dir.split(os.path.sep)
# find last occurence of 'tests' directory on path
test_index_last = curr_dir_list[::-1].index('tests')
test_dir = os.path.sep.join(curr_dir_list[:-test_index_last])
sys.path.insert(0, test_dir)
print sys.path

import test_common
import pytest
import json
import inspect


def performance(default_config, configs):
    # TODO add writing output to json (fixture)
    def wrap(test_function):

        def wrapped_test_function(self, clients, json_output, suite_report):
            case_report = test_function.__name__
            case_report = CaseReport(case_report, default_config['description'])

            for case_config_name, case_config in configs.items():
                merged_config = merge_configs(case_config, default_config)
                config_report = ConfigReport(case_config_name,
                                             merged_config['description'],
                                             merged_config['repeats'])
                for repeat in range(merged_config['repeats']):
                    test_function(self, clients, merged_config['parameters'])

                case_report.add_to_report('configs', config_report)
            suite_report.add_to_report('cases', case_report)
            print json_output.report

        return wrapped_test_function

    return wrap


@pytest.fixture(scope="session")
def json_output(request):
    performance_report = PerformanceReport("performance",
                                           "repository_TODO",   # TODO
                                           "commit_TODO",       # TODO
                                           "branch_TODO")       # TODO

    def fin():
        f = open(test_common.performance_output, 'w')
        f.write(json.dumps(performance_report.report))

    request.addfinalizer(fin)
    return performance_report

# todo MAYBE CLASS IS NOT NEEDED
class TestPerformance:

    @pytest.fixture(scope="module")
    def suite_report(self, request, json_output):
        report = SuiteReport(test_common
                             .get_test_name(inspect.getfile(self.__class__)),
                             "suite_description_TODO",  # TODO
                             "suite_copyright_TODO",    # TODO
                             "suite_authors_TODO")      # TODO

        def fin():
            json_output.add_to_report("suites", report)

        request.addfinalizer(fin)
        return report

    @pytest.fixture(scope="module", params=test_common
                    .get_json_files(test_common.performance_env_dir))
    def env_description_file(self, request):
        """This fixture must be overridden in performance test module if you
        want to start tests from given module with different environments that
        those defined in performance/environments directory
        """
        return request.param

    @pytest.fixture(scope="module")
    def environment(self, request, env_description_file):
        print "RUNNING ENV_FIXTURE"
        # logdir = make_logdir(acceptance_logdir, get_test_name(__file__))
        # env = run_env_up_script("env_up.py", [
        #     '-l', logdir, env_description_file
        # ])
        #
        # def fin():
        #     docker.remove(env['docker_ids'], force=True, volumes=True)
        #
        # request.addfinalizer(fin)
        # return env
        return env_description_file

    # TODO this fixture is similiar to client_ids in cucumber tests
    # TODO it should be moved to common conftest.py
    @pytest.fixture(scope="module")
    def clients(self, environment, env_description_file):
        print "RUNNING CLIENT_FIXTURE"
        # client_dockers = environment['client_nodes']
        # # current version is for environment with one OZ
        # oz_node = environment['oz_worker_nodes'][0]
        # set_dns(environment)
        # client_data = environment['client_data']
        # mounted_clients = {}
        # for client_docker in client_dockers:
        #     client_host, _sep, _timestamp = client_docker.partition('.')
        #     for client in client_data[client_host].keys():
        #         cookie = get_cookie(env_description_file, oz_node)
        #         token = get_token(client_data[client_host][client]['token_for'],
        #                           oz_node, cookie)
        #         token_path = "/tmp/token"
        #         client_name = client_data[client_host][client]['client_name']
        #         mount_path = client_data[client_host][client]['mounting_path']
        #         mounted_clients[client_name] = Client(client_docker, mount_path)
        #         data = client_data[client_host][client]
        #         cmd = ('mkdir -p {mount_path}'
        #                ' && export GLOBAL_REGISTRY_URL={gr_domain}'
        #                ' && export PROVIDER_HOSTNAME={op_domain}'
        #                ' && export X509_USER_CERT={user_cert}'
        #                ' && export X509_USER_KEY={user_key}'
        #                ' && echo {token} > {token_path}'
        #                ' && gdb oneclient -batch -return-child-result -ex \'run --authentication token --no_check_certificate {mount_path} < {token_path}\' -ex \'bt\' 2>&1'
        #                ' && rm {token_path}').format(
        #                 mount_path=mount_path,
        #                 gr_domain=data['zone_domain'],
        #                 op_domain=data['op_domain'],
        #                 user_cert=data['user_cert'],
        #                 user_key=data['user_key'],
        #                 token=token,
        #                 token_path=token_path)
        #
        #         run_cmd('root', mounted_clients[client_name], cmd)
        # return mounted_clients
        return environment
