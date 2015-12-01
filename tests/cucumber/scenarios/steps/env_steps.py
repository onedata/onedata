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

# these commands set up paths to scripts from 'bamboos'
curr_dir = os.path.dirname(os.path.realpath(__file__))

curr_dir_list = curr_dir.split('/')
ind = curr_dir_list.index('onedata')
root_dir = '/'.join(curr_dir_list[:ind+1])

docker_dir = os.path.join(root_dir, 'bamboos', 'docker')
sys.path.insert(0, docker_dir)

from environment import docker, env


@given(parsers.parse('environment is defined in {env_json}'))
def env_file(env_json, context):
    """
    Remembers the environment filename.
    """
    context.env_json = env_json


@given("environment is up", scope="module")
def environment(request, context):
    """
    Sets up environment and returns environment description.
    """
    curr_path = os.path.dirname(os.path.abspath(__file__))
    env_path = os.path.join(curr_path, '..', '..', 'environments', context.env_json)
    env_desc = env.up(env_path)

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
