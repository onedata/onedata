"""Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements common functions for handling test environment.
"""
from environment import docker
from pytest_bdd import given, then
from pytest_bdd import parsers

from tests import *


@given("environment is up")
def environment_up(environment):
    return environment


@then(parsers.parse('{number:d} nodes are up'))
def check_nodes(environment, number):
    """
    Checks whether environment consists of 'number' nodes.
    """
    assert number == len(environment['docker_ids'])
