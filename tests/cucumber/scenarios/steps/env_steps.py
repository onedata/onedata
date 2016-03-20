"""Author: Piotr Ociepka
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements common functions for handling test environment.
"""

from pytest_bdd import parsers
import pytest
from pytest_bdd import (given, when, then)

import os
import sys
import shutil

# these commands add 'tests' to path to make it possible
# to import 'test_common'
curr_dir = os.path.dirname(os.path.realpath(__file__))
curr_dir_list = curr_dir.split(os.path.sep)
# find last occurence of 'tests' directory on path
test_index_last = curr_dir_list[::-1].index('tests')
test_dir = os.path.sep.join(curr_dir_list[:-test_index_last])
sys.path.insert(0, test_dir)

import test_common
from environment import common, docker, env


@given(parsers.parse('environment is defined in {env_json}'))
def env_file(env_json, context):
    """
    Remembers the environment filename.
    """
    env_json = str(env_json)
    context.env_path = os.path.join(test_common.cucumber_env_dir, env_json)
    context.env_json = env_json


@given("storage directories are empty")
def clear_storages(context):
    curr_path = os.path.dirname(os.path.abspath(__file__))
    env_path = os.path.join(curr_path, '..', '..', 'environments', context.env_json)
    config = common.parse_json_config_file(env_path)
    for _, os_config in config['os_configs'].iteritems():
        for storage in os_config['storages']:
            shutil.rmtree(storage, ignore_errors=True)


@given("environment is up", scope="module")
def environment(request, context):
    """
    Sets up environment and returns environment description.
    """
    curr_path = os.path.dirname(os.path.abspath(__file__))
    env_path = os.path.join(curr_path, '..', '..', 'environments', context.env_json)

    feature_name = request.module.__name__
    logdir = test_common.make_logdir(
            test_common.cucumber_logdir,
            os.path.join(context.env_json.split(".")[0], feature_name))
    env_desc = test_common.run_env_up_script("env_up.py", ['-l', logdir, env_path])

    def fin():
        docker.remove(request.environment['docker_ids'], force=True, volumes=True)

    request.addfinalizer(fin)
    request.environment = env_desc
    return env_desc


@then(parsers.parse('{number:d} nodes are up'))
def check_nodes(environment, number):
    """
    Checks whether environment consists of 'number' nodes.
    """
    assert number == len(environment['docker_ids'])
