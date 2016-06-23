"""Common steps for Oneprovider.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.steps import onezone_logged_in_common as onezone_session
from tests.gui.steps import onezone_before_login as onezone_no_session

from pytest_bdd import given, parsers


# @given(parsers.parse('''I'm logged into Oneprovider "{provider}" as development user "{user}"'''))
# def logged_in_dev_to_oneprovider(selenium, base_url, user, provider):
#     onezone_no_session.login_dev_onezone_with_url(selenium, base_url, user)
#     onezone_session.uncollapse_main_accordion(selenium, 'providers')
#     onezone_session.go_to_provider(selenium, provider)
#     pass
