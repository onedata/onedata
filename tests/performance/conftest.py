from tests.test_common import (make_logdir, get_test_name, get_json_files,
                               performance_output, performance_env_dir,
                               acceptance_logdir, run_env_up_script,
                               performance_logdir)

from tests.cucumber.scenarios.steps.common import Client, run_cmd
from tests.cucumber.scenarios.steps.auth_steps import (set_dns, get_cookie,
                                                       get_token)

import os
from utils import *
import pytest
import json
import inspect
import time

from environment import docker


def performance(default_config, configs):
    def wrap(test_function):

        def wrapped_test_function(self, clients, json_output, suite_report):
            test_case_report = test_function.__name__
            test_case_report = TestCaseReport(test_case_report,
                                              default_config['description'])

            for config_name, config in configs.items():

                merged_config = update_dict(default_config, config)
                config_report = ConfigReport(config_name,
                                             merged_config['description'],
                                             merged_config['repeats'])

                config_report.add_to_report(
                        'parameters',
                        dict_to_list(merged_config.get('parameters', {}))
                )

                test_result_report = TestResultReport()
                max_repeats = merged_config['repeats']
                succces_rate = merged_config['success_rate']
                repeats = 0
                failed_repeats = 0
                successful_repeats = 0
                failed_details = {}
                while repeats < max_repeats:

                    try:
                        test_results = test_function(
                                self, clients,
                                merged_config.get('parameters', {}))
                    except Exception as e:
                        failed_repeats += 1
                        failed_details[str(repeats)] = str(e)
                    else:
                        test_results = ensure_list(test_results)
                        test_result_report.add_single_test_results(
                                test_results, repeats)
                        successful_repeats += 1
                    finally:
                        repeats += 1

                config_report.add_to_report('completed', int(time.time()))
                config_report.add_to_report('successful_repeats', successful_repeats)
                test_result_report.prepare_report()
                config_report.add_to_report('successful_repeats_details',
                                            test_result_report.details)
                config_report.add_to_report('successful_repeats_summary',
                                            test_result_report.summary)
                config_report.add_to_report('successful_repeats_average',
                                            test_result_report.average)
                config_report.add_to_report("failed_repeats_details", failed_details)

                test_case_report.add_to_report('configs', config_report)

                if is_success_rate_satisfied(successful_repeats, failed_repeats, succces_rate):
                    suite_report.add_to_report('cases', test_case_report)
                    pytest.fail("Test suite: {suite} failed because of too "
                                "many failures: {failures}"
                                .format(suite=suite_report.name,
                                        failures=failed_repeats))

            suite_report.add_to_report('cases', test_case_report)

        return wrapped_test_function

    return wrap


@pytest.fixture(scope="session")
def json_output(request):
    performance_report = PerformanceReport("performance", get_repository(),
                                           get_commit(), get_branch_name())

    def fin():
        if not os.path.exists(performance_logdir):
            os.makedirs(performance_logdir)
        f = open(performance_output, 'w')
        f.write(json.dumps(performance_report.report))

    request.addfinalizer(fin)
    return performance_report


class TestPerformance:
    @pytest.fixture(scope="module")
    def suite_report(self, request, env_report):
        module = inspect.getmodule(self.__class__)
        name = get_test_name(inspect.getfile(self.__class__))
        report = SuiteReport(name,
                             get_suite_description(module),
                             get_copyright(module),
                             get_authors(module))

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

    @pytest.fixture(scope="module", params=get_json_files(performance_env_dir))
    def env_description_file(self, request):
        """This fixture must be overridden in performance test module if you
        want to start tests from given module with different environments that
        those defined in performance/environments directory
        """
        return request.param

    @pytest.fixture(scope="module")
    def environment(self, request, env_description_file):
        logdir = make_logdir(acceptance_logdir, get_test_name(__file__))
        env = run_env_up_script("env_up.py", [
            '-l', logdir, env_description_file
        ])

        def fin():
            docker.remove(env['docker_ids'], force=True, volumes=True)

        request.addfinalizer(fin)
        return env

    # TODO this fixture is similiar to client_ids in cucumber tests
    # TODO it should be moved to common conftest.py
    @pytest.fixture(scope="module")
    def clients(self, environment, env_description_file):
        client_dockers = environment['client_nodes']
        # current version is for environment with one OZ
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


def is_success_rate_satisfied(successful_repeats, failed_repeats, rate):
    return rate / 100.0 > float(successful_repeats) / (successful_repeats + failed_repeats)