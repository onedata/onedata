"""Steps for access tokens management using REST API.
"""

from collections import defaultdict

from pytest import fixture
from pytest_bdd import given, parsers

from .common import get_oz_user_api


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@fixture
def access_tokens():
    """Mapping browser/user name to its list of access tokens,

    E.g. {browser1: [SETrw33rwef..., ASDRWER45...}
    """
    return defaultdict(set)


@given(parsers.parse('user of {browser_id} already created '
                     'access token using REST API'))
def create_access_token_for_user(browser_id, users, hosts, access_tokens):
    user_info = users[browser_id]
    host = hosts['onezone'][user_info.zone]
    rest_client = get_oz_user_api(user_info.username, user_info.password, host)
    access_tokens[browser_id].add(rest_client.generate_client_token().token)
