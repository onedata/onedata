import os
import sys
import time
import subprocess
import pytest
import ast

_script_dir = os.path.dirname(os.path.realpath(__file__))

# Define variables for use in tests
project_dir = os.path.dirname(_script_dir)
appmock_dir = os.path.join(project_dir, 'appmock')
bamboos_dir = os.path.join(project_dir, 'bamboos')
docker_dir = os.path.join(bamboos_dir, 'docker')
test_dir = os.path.join(project_dir, "tests")
cucumber_dir = os.path.join(test_dir, "cucumber")
acceptance_dir = os.path.join(test_dir, "acceptance")
performance_dir = os.path.join(test_dir, "performance")
cucumber_logdir = os.path.join(cucumber_dir, "logs")
acceptance_logdir = os.path.join(acceptance_dir, "logs")
performance_logdir = os.path.join(performance_dir, "logs")
performance_env_dir = os.path.join(performance_dir, "environments")
performance_output = os.path.join(performance_logdir, "performance.json")
# TODO after uncommenting env_up in tests, unomment above line and delete below line
# performance_output = os.path.join(performance_dir, "performance.json")
example_env_dir = os.path.join(bamboos_dir, "example_env")

# Append useful modules to the path
sys.path = [project_dir, docker_dir] + sys.path


def get_test_name(filename):
    filename = filename.split("/")[-1]
    (test_name, ext) = filename.split(".")
    return test_name


def get_test_data_dir(test_file):
    """Returns absolute path to directory <test_file>_data"""
    return "{0}_data".format(os.path.realpath(test_file).rstrip('.py'))


def get_logdir_name(root_dir, test_name):
    timestamp = str(time.time())
    return os.path.join(root_dir, ".".join([test_name, timestamp]))


def make_logdir(root_dir, test_name):
    """Logdir is created if it doesn't exist."""
    name = get_logdir_name(root_dir, test_name)
    if not os.path.exists(name):
        os.makedirs(name)
    return name


def get_json_files(dir):
    """Gets all .json files from given directory
    Returns list of files' absolute paths"""
    jsons = []
    for file in os.listdir(dir):
        if file.endswith(".json"):
            jsons.append(os.path.join(dir, file))
    return jsons


def run_env_up_script(script, args=[]):
    """Runs given script to bring up test environment.
    Script must be located in docker_dir directory (see test_common.py)
    If script fails, functions skips test.
    """
    cmd = [os.path.join(docker_dir, script)]
    if args:
        cmd.extend(args)
    try:
        output = subprocess.check_output(cmd)
    except subprocess.CalledProcessError:
        pytest.skip(script + " script failed")

    stripped_output = strip_output_logs(output)
    # get dict from string
    output_dict = ast.literal_eval(stripped_output)
    return output_dict


def strip_output_logs(output):
    """Strips logs such as 'Add user ... ' etc
    from output, returns env description
    """
    return output.strip().split('\n')[-1]


