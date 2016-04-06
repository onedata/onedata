class Report:

    def __init__(self, name):
        self.name = name
        self.report = {name: {}}

    def add_to_report(self, key, value):
        print "ADD TO REPORT ", key
        if isinstance(value, Report):
            self.add_nested_report(key, value)
        else:
            self.report[self.name][key] = value

    def add_nested_report(self, key, value):
        if key == "suites":
            f = open('debug', 'a')
            f.write("NESTED " + value.report.keys()[0])
        print "BEFORE ADD: ", self.report
        print "ADDING KEY: ", key
        print "ADDING VALUE: ", value.report
        self.report[self.name][key][value.name] = value.report[value.name]
        # self.report[self.name][key][value.name].update(value.report)
        print "AFTER ADD: ", self.report


class PerformanceReport(Report):

    def __init__(self, name, repository, commit, branch):
        Report.__init__(self, name)
        self.report[name] = {'suites': {}}
        self.add_to_report('repository', repository)
        self.add_to_report('commit', commit)
        self.add_to_report('branch', branch)


class SuiteReport(Report):

    def __init__(self, name, description, copyright, authors):
        Report.__init__(self, name)
        self.add_to_report('name', name)
        self.add_to_report('description', description)
        self.add_to_report('copyright', copyright)
        self.add_to_report('authors', authors)
        self.add_to_report('cases', {})
        # print "SUITE: ", self.report


class CaseReport(Report):

    def __init__(self, name, description,):
        Report.__init__(self, name)
        self.add_to_report('name', name)
        self.add_to_report('description', description)
        self.add_to_report('configs', {})
        # print "CASE: ", self.report


class ConfigReport(Report):
    def __init__(self, name, description, repeats):
        Report.__init__(self, name)
        self.add_to_report('name', name)
        self.add_to_report('description', description)
        self.add_to_report('repeats', repeats)


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
