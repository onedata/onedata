"""This file contains utility functions for performance tests.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import sys
import itertools
import time
import pytest


def performance(default_config, configs):
    """This function is meant to run performance test. It allows to start
    test cases multiple times and for many configs. It should be used as
    decorator to test function.
    :param default_config: dictionary with keys:
        - repeats - how many times test should be started for each config
        - success_rate - if rate of successful tests is lower than this
          parameter, whole test will fail
        - parameters - dictionary with parameters that are specific for given
          test case
        - description - description of test case
    :param configs: dictionary of configs. For each of this configs test case
                    will be started
    """
    def wrap(test_function):

        def wrapped_test_function(self, context, clients, suite_report):
            test_case_report = test_function.__name__
            test_case_report = TestCaseReport(test_case_report,
                                              default_config['description'])

            failed = False
            error_msg = ""

            for config_name, config in configs.items():
                flushed_print("Running {}".format(config['description']))

                merged_config = update_dict(default_config, config)
                config_report = ConfigReport(config_name,
                                             merged_config['description'],
                                             merged_config['repeats'])

                config_report.add_to_report('parameters',
                                            dict_to_list(merged_config.get('parameters', {})))

                test_result_report = ResultReport()
                max_repeats = merged_config['repeats']
                succes_rate = merged_config['success_rate']
                repeats = 0
                failed_repeats = 0
                successful_repeats = 0
                failed_details = {}
                while repeats < max_repeats:
                    flushed_print("RUN {}/{}".format(repeats+1, max_repeats))

                    try:
                        test_results = test_function(self, context, clients,
                                                     merged_config
                                                     .get('parameters', {}))
                    except Exception as e:
                        flushed_print("\t\tTestcase failed beceause of: " + str(e))
                        failed_repeats += 1
                        failed_details[str(repeats)] = str(e)
                    else:
                        test_results = ensure_list(test_results)
                        test_result_report.add_single_test_results(test_results,
                                                                   repeats)
                        successful_repeats += 1
                    finally:
                        repeats += 1

                config_report.add_to_report('completed', int(time.time()))
                config_report.add_to_report('successful_repeats', successful_repeats)
                test_result_report.prepare_report()
                config_report.add_to_report('successful_repeats_details', test_result_report.details)
                config_report.add_to_report('successful_repeats_summary', test_result_report.summary)
                config_report.add_to_report('successful_repeats_average', test_result_report.average)
                config_report.add_to_report("failed_repeats_details", failed_details)

                test_case_report.add_to_report('configs', config_report)

                if not is_success_rate_satisfied(successful_repeats, failed_repeats, succes_rate):
                    error_msg = ("Test suite: {suite} failed because of too "
                                 "many failures: {failures}"
                                 ).format(suite=suite_report.name,
                                          failures=failed_repeats)
                    failed = True
                    break

            suite_report.add_to_report('cases', test_case_report)
            if failed:
                pytest.fail(error_msg)

        return wrapped_test_function

    return wrap


class Report:
    def __init__(self, name):
        self.name = name
        self.report = {name: {}}

    def add_to_report(self, key, value):
        if isinstance(value, Report):
            self.add_nested_report(key, value)
        else:
            self.report[self.name][key] = value

    def add_nested_report(self, key, value):
        if value.name not in self.report[self.name][key].keys():
            self.report[self.name][key][value.name] = value.report[value.name]
        else:
            self.report[self.name][key][value.name] = \
                update_dict(self.report[self.name][key][value.name], value.report[value.name])


class PerformanceReport(Report):
    def __init__(self, name, repository, commit, branch):
        Report.__init__(self, name)
        self.report[name] = {'envs': {}}
        self.add_to_report('repository', repository)
        self.add_to_report('commit', commit)
        self.add_to_report('branch', branch)


class EnvironmentReport(Report):
    def __init__(self, name):
        Report.__init__(self, name)
        self.add_to_report('name', name)
        self.add_to_report('suites', {})


class SuiteReport(Report):
    def __init__(self, name, description, copyright, authors):
        Report.__init__(self, name)
        self.add_to_report('name', name)
        self.add_to_report('description', description)
        self.add_to_report('copyright', copyright)
        self.add_to_report('authors', authors)
        self.add_to_report('cases', {})


class TestCaseReport(Report):
    def __init__(self, name, description, ):
        Report.__init__(self, name)
        self.add_to_report('name', name)
        self.add_to_report('description', description)
        self.add_to_report('configs', {})


class ConfigReport(Report):
    def __init__(self, name, description, repeats):
        Report.__init__(self, name)
        self.add_to_report('name', name)
        self.add_to_report('description', description)
        self.add_to_report('repeats_number', repeats)
        self.add_to_report('parameters', [])
        self.add_to_report('successful_repeats_summary', [])
        self.add_to_report('successful_repeats_details', [])
        self.add_to_report('successful_repeats_average', [])
        self.add_to_report('failed_repeats_details', {})


class Result:
    def __init__(self, name, value, description, unit=""):
        self.name = name
        self.value = value
        self.description = description
        self.unit = unit


class ResultReport:

    def __init__(self):
        self.details = {}
        self.summary = {}
        self.average = {}
        self.num = 0

    def prepare_report(self):
        for key in self.details:
            avg = float(self.summary[key]['value'])/self.num
            self.average[key]['value'] = avg
        self.details = dict_to_list(self.details)
        self.summary = dict_to_list(self.summary)
        self.average = dict_to_list(self.average)

    def add_single_test_results(self, test_results, repeat):
        for test_result in test_results:
            if test_result.name not in self.details.keys():
                self.add_new(test_result, repeat)
            else:
                self.add_existing(test_result, repeat)
        self.num += 1

    def add_new(self, test_result, repeat):
        name = test_result.name
        val = test_result.value
        new_result = {
            'name': name,
            'description': test_result.description,
            'unit': test_result.unit,
        }
        self.details[name] = new_result
        self.details[name]['value'] = {str(repeat): val}
        self.summary[name] = dict(new_result)
        self.summary[name]['value'] = val
        self.average[name] = dict(new_result)

    def add_existing(self, test_result, repeat):
        name = test_result.name
        val = test_result.value
        self.details[name]['value'].update({str(repeat): val})
        self.summary[name]['value'] += val


def update_dict(base, updating):
    new_dict = dict(base)
    for key in updating.keys():
        if key in base.keys() and \
                isinstance(updating[key], dict) and \
                isinstance(new_dict[key], dict):

            new_dict[key] = update_dict(new_dict[key], updating[key])
        else:
            new_dict[key] = updating[key]
    return new_dict


def dict_to_list(dict):
    list = []
    for key in dict.keys():
        new_elem = dict[key]
        new_elem['name'] = key
        list.append(new_elem)
    return list


def ensure_list(elem):
    if not elem:
        return []
    if not isinstance(elem, list):
        elem = [elem]
    return elem


def generate_configs(params, description_skeleton):
    """This function generates all combinations of given parameters. Format of
    returnev value is appropriate for @performance decorator
    :param description_skeleton: skeleton of config description, it will be
    filled with parameter values
    :param params: dictionary of parameters in form
    {"param_name": [val1, val2, val3]}
    """
    keys = params.keys()
    configs = {}
    combinations = itertools.product(*params.values())

    for i, combination in enumerate(combinations):
        conf_name = 'config{}'.format(i)
        configs[conf_name] = dict()
        new_params = dict(zip(keys, combination))
        description = description_skeleton.format(**new_params)
        for key, value in new_params.items():
            new_params[key] = {'value': value}
        configs[conf_name].update({
            'parameters': new_params,
            'description': description
        })
    return configs


def is_success_rate_satisfied(successful_repeats, failed_repeats, rate):
    return rate * (successful_repeats + failed_repeats) <= 100 * successful_repeats


def flushed_print(msg):
    print msg
    sys.stdout.flush()
