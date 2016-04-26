import os
import sys
import time
import subprocess
import pytest
import ast

# env_up log files
PREPARE_ENV_LOG_FILE = "prepare_test_environment.log"
PREPARE_ENV_ERROR_LOG_FILE = "prepare_test_environment_error.log"

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
default_cucumber_env_dir = os.path.join(cucumber_dir, "default_environments")
custom_cucumber_env_dir = os.path.join(cucumber_dir, "custom_environments")
cucumber_logdir = os.path.join(cucumber_dir, "logs")
acceptance_logdir = os.path.join(acceptance_dir, "logs")
performance_logdir = os.path.join(performance_dir, "logs")
performance_env_dir = os.path.join(performance_dir, "environments")
performance_output = os.path.join(performance_logdir, "performance.json")
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


def get_json_files(dir, relative=False):
    """Gets all .json files from given directory
    Returns list of files' absolute paths"""
    jsons = []
    for file in os.listdir(dir):
        if file.endswith(".json"):
            if not relative:
                jsons.append(os.path.join(dir, file))
            else:
                jsons.append(file)
    return jsons


def env_description_files(dir, *envs):
    """Returns list of absolute paths to environment
    description files envs from  dir"""
    envs_absolute_paths = []
    for env in envs:
        envs_absolute_paths.append(os.path.join(dir, env))
    return envs_absolute_paths


def env_name(env_description_file_path):
    """Returns name of environment based on environment description file name
    i.e
    for file 'env1.json' returns 'env1'
    for file '/abs/path/env2.json return 'env2'
    """
    return os.path.splitext(os.path.basename(env_description_file_path))[0]


def run_env_up_script(script, config=None, logdir=None, args=[]):
    """Runs given script to bring up test environment.
    Script must be located in docker_dir directory (see test_common.py)
    If script fails, functions skips test.
    """
    cmd = [os.path.join(docker_dir, script)]

    if logdir:
        cmd.extend(['-l', logdir])
    if args:
        cmd.extend(args)
    if config:
        cmd.append(config)

    try:
        output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
    except Exception as e:
        if isinstance(e, subprocess.CalledProcessError):
            err_msg = e.output
        else:
            err_msg = str(e)
        if not logdir:
            # even if script doesn't have logdir option we want logs from
            # executing this script
            logdir = make_logdir(acceptance_dir, script)
        logfile_error_path = os.path.join(logdir, PREPARE_ENV_ERROR_LOG_FILE)
        save_log_to_file(logfile_error_path, err_msg)
        pytest.skip("{script} script failed because of {reason}".format(
            script=script,
            reason=err_msg
        ))

    stripped_output = strip_output_logs(output)

    # get dict from string
    output_dict = ast.literal_eval(stripped_output)
    if logdir:
        logfile_path = os.path.join(logdir, PREPARE_ENV_LOG_FILE)
        logfile = open(logfile_path, 'w')
        logfile.write(stripped_output)
        logfile.close()
    return output_dict


def strip_output_logs(output):
    """Strips logs such as 'Add user ... ' etc
    from output, returns env description
    """
    return output.strip().split('\n')[-1]


def save_log_to_file(file_path, log):
    f = open(file_path, 'w')
    f.write(log)
    f.close()
