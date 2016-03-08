import os
import sys
import time

_script_dir = os.path.dirname(os.path.realpath(__file__))

# Define variables for use in tests
project_dir = os.path.dirname(_script_dir)
appmock_dir = os.path.join(project_dir, 'appmock')
docker_dir = os.path.join(project_dir, 'bamboos', 'docker')
cucumber_logdir = os.path.join(project_dir, "tests/cucumber/logs")
acceptance_logdir = os.path.join(project_dir, "tests/acceptance/logs")
example_env_dir = os.path.join(project_dir, "bamboos", "example_env")

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
