"""This module contains utility functions to be used in acceptance tests."""
import logging
import traceback

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from tests import *
from tests.utils.path_utils import make_logdir, save_log_to_file

from environment.common import parse_json_config_file

import ast
import re
import subprocess
import pytest
import os
import inspect
import time

ENV_UP_RETRIES_NUMBER = 5


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


def run_env_up_script(script, config=None, logdir=None, args=[], skip=True,
                      retries=ENV_UP_RETRIES_NUMBER):
    """Runs given script to bring up test environment.
    Script must be located in docker_dir directory (see test_common.py)
    If script fails, functions skips test (if skip=True).
    """
    cmd = [os.path.join(DOCKER_DIR, script)]

    if logdir:
        cmd.extend(['-l', logdir])
    if args:
        cmd.extend(args)
    if config:
        cmd.append(config)

    output = ""
    try:
        output = retry_running_cmd_until(cmd, retries=retries)
    except Exception as e:
        if isinstance(e, subprocess.CalledProcessError):
            err_msg = e.output
        else:
            err_msg = str(e)
        if not logdir:
            # even if script doesn't have logdir option we want logs from
            # executing this script
            logdir = make_logdir(ENV_UP_DIR, script)
        logfile_error_path = os.path.join(logdir, PREPARE_ENV_ERROR_LOG_FILE)
        save_log_to_file(logfile_error_path, err_msg)
        msg = "{script} script failed because of {reason}"\
            .format(script=script, reason=err_msg)
        pytest.skip(msg) if skip else pytest.fail(msg)

    stripped_output = strip_output_logs(output)

    # get dict from string
    output_dict = ast.literal_eval(stripped_output)
    if logdir:
        logfile_path = os.path.join(logdir, PREPARE_ENV_LOG_FILE)
        logfile = open(logfile_path, 'w')
        logfile.write(stripped_output)
        logfile.close()
    return output_dict


def retry_running_cmd_until(cmd, retries=0):
    """Retries running command
    :param cmd: command to be run
    :type cmd: <type 'str'>
    :param retries: number of times that command will be retried to run
    :type retries: <type 'int'>
    """

    try:
        output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
    except Exception as e:
        print """{0}
Number of retries left: {1}
""".format(e, retries)

        if retries > 0:
            time.sleep(1)
            output = retry_running_cmd_until(cmd, retries - 1)
        else:
            raise
    return output


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


def set_dns(onedata_environment):
    with open("/etc/resolv.conf", "w") as conf:
        dns = onedata_environment['dns']
        conf.write("nameserver " + dns)


def get_token(token, user, oz_node, cookie):
    if token == "bad token":
        token = "bad_token"
    elif token == "token":
        token = subprocess.check_output([
            os.path.join(UTILS_DIR, 'get_token.escript'), oz_node, user, cookie],
            stderr=subprocess.STDOUT)
    return token


def get_oz_cookie(config_path, oz_name, node_name=True):
    return get_cookie(config_path, oz_name, 'zone_domains', node_name)


def get_op_cookie(config_path, op_name, node_name=True):
    return get_cookie(config_path, op_name, 'provider_domains', node_name)


def get_cookie(config_path, name, domain, node_name=True):
    """Reads erlang cookie from file at config path.
    node_name = True means that argument name is a node name, otherwise
    it is a domain name.
    """
    if '@' in name:
        _, _, name = name.partition('@')
    if node_name:
        domain_name = name.split(".")[1]
    else:
        domain_name = name.split(".")[0]
    config = parse_json_config_file(config_path)
    cm_config = config[domain][domain_name]['cluster_manager']
    key = cm_config.keys()[0]
    return str(cm_config[key]['vm.args']['setcookie'])


def get_storages(config_path, provider_id):
    config = parse_json_config_file(config_path)
    cfg = config['provider_domains'][provider_id]['os_config']
    return config['os_configs'][cfg]['storages']


def get_matching_op_erl_node(name, env):
    return [w for w in env['op_worker_nodes'] if name in w][0]


def get_first_op_erl_node(domain, env):
    return get_first_erl_node(domain, env, 'op_worker_nodes')


def get_first_oz_erl_node(domain, env):
    return get_first_erl_node(domain, env, 'oz_worker_nodes')


def get_first_erl_node(domain, env, key):
    return [node for node in env[key] if node.endswith(domain)][0]


def get_first_op_worker(domain, env):
    return hostname(get_first_op_erl_node(domain, env))


def get_first_oz_worker(domain, env):
    return hostname(get_first_oz_erl_node(domain, env))


def hostname(erl_node):
    return erl_node.split('@')[-1]


def get_domain(node):
    if '@' in node:
        node = hostname(node)
    return node.split('.', 1)[-1]


def log_exception():
    extracted_stack = traceback.format_exc(10)
    logging.error(extracted_stack)


def assert_generic(expression, should_fail, *args, **kwargs):
    if should_fail:
        assert_false(expression, *args, **kwargs)
    else:
        assert_(expression, *args, **kwargs)


def assert_(expression, *args, **kwargs):
    assert_result = expression(*args, **kwargs)
    assert assert_result


def assert_false(expression, *args, **kwargs):
    assert_result = expression(*args, **kwargs)
    assert not assert_result
