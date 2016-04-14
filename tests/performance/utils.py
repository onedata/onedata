import itertools
import subprocess
import os
import re


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
        self.report[self.name][key][value.name] = value.report[value.name]


class PerformanceReport(Report):
    def __init__(self, name, repository, commit, branch):
        Report.__init__(self, name)
        # self.report[name] = {'suites': {}}
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


class TestResult:
    def __init__(self, name, value, description, unit=""):
        self.name = name
        self.value = value
        self.description = description
        self.unit = unit


class TestResultReport:

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
    if not isinstance(elem, list):
        elem = [elem]
    return elem


def generate_configs(params, description_skeleton):
    keys = params.keys()
    configs = {}
    i = 0
    combinations = itertools.product(*params.values())

    for combination in combinations:
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
        i += 1

    return configs


def get_branch_name():
    return subprocess.check_output(["git", "rev-parse", "--abbrev-ref", "HEAD"])


def get_commit():
    return subprocess.check_output(["git", "rev-parse", "HEAD"])


def get_repository():
    toplevel = subprocess.check_output(['git', 'rev-parse', '--show-toplevel'])
    return os.path.basename(toplevel).strip()


def get_copyright(mod):
    return mod.__copyright__ if hasattr(mod, '__copyright__') else ''


def get_authors(mod):
    author = mod.__author__ if hasattr(mod, '__author__') else ''
    return re.split(r'\s*,\s*', author)


def get_suite_description(mod):
    return mod.__doc__
