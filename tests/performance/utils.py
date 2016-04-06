import json


class PerformanceReport:

    def __init__(self, name):
        self.name = name
        self.report = {name: {}}

    def add_to_report(self, key, value):
        print "ADD TO REPORT ", key
        if isinstance(value, PerformanceReport):
            self.add_nested_report(key, value)
        else:
            self.report[self.name][key] = value

    def add_nested_report(self, key, value):
        print "BEFORE ADD: ", self.report
        print "ADDING KEY: ", key
        print "ADDING VALUE: ", value.report
        self.report[self.name][key][value.name] = value.report[value.name]
        # self.report[self.name][key][value.name].update(value.report)
        print "AFTER ADD: ", self.report


class JsonReport(PerformanceReport):

    def __init__(self, name):
        PerformanceReport.__init__(self, name)
        self.report[name] = {'suites': {}}

    def dump(self, file_name):
        f = open(file_name, 'w')
        f.write(json.dumps(self.report))


class Suite(PerformanceReport):

    def __init__(self, name, description, copyright, authors):
        PerformanceReport.__init__(self, name)
        self.add_to_report('name', name)
        self.add_to_report('description', description)
        self.add_to_report('copyright', copyright)
        self.add_to_report('authors', authors)
        self.add_to_report('cases', {})
        print "SUITE: ", self.report


class Case(PerformanceReport):

    def __init__(self, name, description,):
        PerformanceReport.__init__(self, name)
        self.add_to_report('name', name)
        self.add_to_report('description', description)
        print "CASE: ", self.report


def merge_configs(config, default_config):
    merged_config = dict(config)
    params_key = 'parameters'
    for key in default_config.keys():
        if key == params_key and params_key in config.keys():
            merged_params = dict(default_config[params_key])
            merged_params.update(config[params_key])
            merged_config[params_key] = merged_params
        elif key != 'description' and key not in merged_config.keys():
            merged_config[key] = default_config[key]
    return merged_config


def suite_name(module):
    return module.split('.')[-1]
