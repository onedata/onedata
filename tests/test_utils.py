from tests.test_common import (docker_dir, acceptance_dir,
                               PREPARE_ENV_ERROR_LOG_FILE, PREPARE_ENV_LOG_FILE)

import ast
import inspect
import sys
import time
import subprocess
import os
import pytest


def config_file(relative_file_path):
    """Returns a path to file located in {test_name}_data directory, where
    {test_name} is name of the test module that called this function.
    example: using test_utils.config_file('my_file') in my_test.py will return
    'tests/my_test_data/my_file'
    """
    caller = inspect.stack()[1]
    caller_mod = inspect.getmodule(caller[0])
    caller_mod_file_path = caller_mod.__file__
    return '{0}_data/{1}'.format(caller_mod_file_path.rstrip('.py'),
                                 relative_file_path)


def run_os_command(cmd, output=True):
    """Runs a given command and returns unicode output.
    The argument may be:
    1) a full command: 'ls -al'
    2) list of strings to be joined with spaces: ['ls' '-al']
    If output is True, function will return output from command, otherwise
    return return code from running command.
    """
    if not (isinstance(cmd, list) or (isinstance(cmd, str))):
        raise ValueError("argument must be a string or a list of strings")
    # Execute the command and remove trailing whitespaces
    if output:
        return subprocess.check_output(cmd).rstrip()
    else:
        with open(os.devnull, 'w') as devnull:
            return subprocess.call(cmd, stdout=devnull, stderr=devnull)


def get_module(name):
    """Returns module object"""
    return sys.modules[name]


def get_function(module, function_name):
    """Returns function object from given module"""
    return getattr(module, function_name)


def get_file_name(file_path):
    """Returns name of file, basing on file_path.
    Name is acquired by removing parent directories from file_path and strippin
    extension.
    i.e. get_file_name("dir1/dir2/file.py") will return "file"
    """
    return os.path.splitext(os.path.basename(file_path))[0]


def get_logdir_name(root_dir, test_name):
    """Returns path to logs directory
    i.e. get_logdir_name("tests/mytest", "test1") will return
    "tests/mytest/test1.<timestamp>"
    """
    timestamp = str(time.time())
    return os.path.join(root_dir, ".".join([test_name, timestamp]))


def make_logdir(root_dir, test_name):
    """Creates logdir if it doesn't exist."""
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
    from output returned by env_up. Returns env description.
    """
    return output.strip().split('\n')[-1]


def save_log_to_file(file_path, log):
    """Saves log to file pointed by file_path"""
    f = open(file_path, 'w')
    f.write(log)
    f.close()