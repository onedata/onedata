"""Test suite for features of Onezone login page.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.steps.common import *
from tests.gui.steps.onezone_before_login import *
from tests.gui.steps.onezone_logged_in_common import *
from tests.gui.steps.oneprovider_common import *
from tests.gui.steps.oneprovider_data import *

import pytest
from pytest_bdd import scenarios, scenario


# --- FEATURES: onezone_login --- #

# @pytest.mark.nondestructive
# @scenario('../features/onezone_login.feature',
#           'Onezone login page renders with proper title')
# def test_oz_page_renders_title():
#     pass
#
#
# @pytest.mark.nondestructive
# @scenario('../features/onezone_login.feature',
#           'Rendering multiple login buttons')
# def test_oz_render_n_login_buttons():
#     pass
#
#
# @pytest.mark.nondestructive
# @scenario('../features/onezone_login.feature',
#           'Rendering particular login buttons')
# def test_oz_render_particular_login_button():
#     pass
#
#
# @pytest.mark.nondestructive
# @scenario('../features/onezone_login.feature',
#           'Showing the development login list')
# def test_oz_show_development_login():
#     pass
#
#
# @pytest.mark.nondestructive
# @scenario('../features/onezone_login.feature',
#           'Logging in with development login')
# def test_login_with_first_development_login_button():
#     pass
#

# --- FEATURES: onezone_gui --- #

# @pytest.mark.nondestructive
# @scenario('../features/onezone_gui.feature',
#           'User can change his alias using valid alias string')
# def test_change_alias():
#     pass

# --- FEATURES: oneprovider_data --- #

# # FIXME: this test fails because of bug
@scenario('../features/oneprovider_data.feature',
          'After failed upload to broken space, the same file can be successfully uploaded to correct space')
def test_upload_fail_and_then_success_one_file():
    pass


@scenario('../features/oneprovider_data.feature',
          'After failed upload some file to broken space, an other file can be successfully uploaded to correct space')
def test_upload_fail_and_then_success_other_files():
    pass

# @scenario('../features/oneprovider_data.feature',
#           'Uploading a file whose size exceeds the space quota should fail')
# def test_upload_too_big_file():
#     pass
#
#
# @scenario('../features/oneprovider_data.feature',
#           'Uploading a file to space should succeed')
# def test_upload_success():
#     pass

# Currently not used, because all tests are explicitly defined
# import other scenarios - note, that nondestructive tests were imported with @scenario
# scenarios('../features/onezone_login.feature')
