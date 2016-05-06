"""This module contains utility functions to be used in acceptance tests."""
from tests import *
from tests.utils.file_utils import make_logdir, save_log_to_file

import ast
import re
import subprocess
import pytest

__author__ = "Jakub Kudzia"


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


def run_env_up_script(script, config=None, logdir=None, args=[]):
    """Runs given script to bring up test environment.
    Script must be located in docker_dir directory (see test_common.py)
    If script fails, functions skips test.
    """
    cmd = [os.path.join(DOCKER_DIR, script)]

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
            logdir = make_logdir(ACCEPTANCE_DIR, script)
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


def get_copyright(mod):
    return mod.__copyright__ if hasattr(mod, '__copyright__') else ''


def get_authors(mod):
    author = mod.__author__ if hasattr(mod, '__author__') else ''
    return re.split(r'\s*,\s*', author)


def get_suite_description(mod):
    return mod.__doc__
