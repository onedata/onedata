"""Module implements steps for handling test environment.
"""
__author__ = "Jakub Kudzia, Piotr Ociepka"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from pytest_bdd import given, then
from pytest_bdd import parsers


@given("environment is up")
def environment_up(onedata_environment):
    return onedata_environment


@then(parsers.re('(?P<number>.*) nodes are up'), converters=dict(number=int))
def check_nodes(onedata_environment, number):
    """
    Checks whether environment consists of 'number' nodes.
    """
    assert number == len(onedata_environment['docker_ids'])
