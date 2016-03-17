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
cucumber_logdir = os.path.join(project_dir, "tests/cucumber/logs")
acceptance_logdir = os.path.join(project_dir, "tests/acceptance/logs")
example_env_dir = os.path.join(bamboos_dir, "example_env")

# Append useful modules to the path
sys.path = [project_dir, docker_dir] + sys.path


def get_test_name(filename):
    filename = filename.split("/")[-1]
    (test_name, ext) = filename.split(".")
    return test_name


def get_logdir_name(root_dir, test_name):
    timestamp = str(time.time())
    return os.path.join(root_dir, ".".join([test_name, timestamp]))


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