"""Test suite for features of Onezone login page.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import sys

from tests.gui.steps.common import *
from tests.gui.steps.modal import *

from tests.gui.steps.onezone_before_login import *
from tests.gui.steps.onezone_spaces import *
from tests.gui.steps.onezone_logged_in_common import *
from tests.gui.steps.onezone_user_alias import *
from tests.gui.steps.onezone_access_tokens import *
from tests.gui.steps.onezone_panel_list import *
from tests.gui.steps.onezone_provider_popup import *
from tests.gui.steps.onezone_providers import *

from tests.gui.steps.oneprovider_common import *
from tests.gui.steps.oneprovider_data import *
from tests.gui.steps.oneprovider_spaces import *
from tests.gui.steps.oneprovider_shares import *
from tests.gui.steps.oneprovider_metadata import *
from tests.gui.steps.oneprovider_file_list import *
from tests.gui.steps.oneprovider_sidebar_list import *

import pytest
from pytest_bdd import scenarios, scenario


BROWSER = ''
for arg in sys.argv:
    if '--driver=' in arg:
        BROWSER = arg.split('=')[1]


# --- FEATURES: all non-destructive (does not change state) ---
scenarios('../features/oneprovider_group_multi.feature')
scenarios('../features/oneprovider_group.feature')
scenarios('../features/onezone_login.feature')
scenarios('../features/onezone_gui.feature')
scenarios('../features/oneprovider_data.feature')
scenarios('../features/oneprovider_space.feature')
scenarios('../features/oneprovider_shares.feature')
scenarios('../features/oneprovider_shares_multi.feature')
scenarios('../features/oneprovider_metadata.feature')

# limit some tests to chrome (due to multiple files upload can be simulated in selenium only in Chrome)
if BROWSER == 'Chrome':
    scenarios('../features/oneprovider_upload_multi_files.feature')

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
