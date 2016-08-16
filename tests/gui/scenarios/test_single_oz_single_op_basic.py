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
from tests.gui.steps.oneprovider_group import *
from tests.gui.steps.oneprovider_spaces import *


import pytest
from pytest_bdd import scenarios, scenario

# --- FEATURES: all non-destructive (does not change state) ---
# scenarios('../features/oneprovider_group.feature')
#scenarios('../features/onezone_login.feature')
#scenarios('../features/onezone_gui.feature')
# scenarios('../features/oneprovider_data.feature')
scenarios('../features/oneprovider_space.feature')
#
#
# # --- FEATURES: oneprovider_data --- #
#
# @pytest.mark.xfail(reason='Fails randomly, need to find out what is a problem', run=False)
# @pytest.mark.destructive
# @scenario('../features/oneprovider_data.feature',
#           'Uploading a small file to space that accepts large files should succeed')
# def test_upload_success():
#     pass
#
