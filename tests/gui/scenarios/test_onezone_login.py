"""Test suite for features of Onezone login page.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.steps.onezone_login import *

import pytest
from pytest_bdd import scenarios, scenario


@pytest.mark.nondestructive
@scenario('../features/onezone_login.feature',
          'Onezone page renders with proper title')
def test_oz_page_renders_title():
    pass


@pytest.mark.nondestructive
@scenario('../features/onezone_login.feature',
          'Rendering multiple login buttons')
def test_oz_render_n_login_buttons():
    pass


@pytest.mark.nondestructive
@scenario('../features/onezone_login.feature',
          'Rendering particular login buttons')
def test_oz_render_particular_login_button():
    pass


@pytest.mark.nondestructive
@scenario('../features/onezone_login.feature',
          'Showing the development login list')
def test_oz_show_development_login():
    pass


# Currently not used, because all tests are explicitly defined
# import other scenarios - note, that nondestructive tests were imported with @scenario
# scenarios('../features/onezone_login.feature')
