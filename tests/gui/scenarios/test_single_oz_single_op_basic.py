"""Test suite for features of Onezone login page.
"""
__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import sys

from tests.gui.steps.common import *
from tests.gui.steps.modal import *


from tests.gui.steps.generic.url import *
from tests.gui.steps.generic.browser_creation import *
from tests.gui.steps.generic.copy_paste import *
from tests.gui.steps.generic.login import *
from tests.gui.steps.generic.account_management import *

from tests.gui.steps.onepanel.common import *
from tests.gui.steps.onepanel.deployment import *
from tests.gui.steps.onepanel.space_support import *

from tests.gui.steps.onezone.logged_in_common import *
from tests.gui.steps.onezone.user_alias import *
from tests.gui.steps.onezone.access_tokens import *
from tests.gui.steps.onezone.data_space_management import *
from tests.gui.steps.onezone.providers import *
from tests.gui.steps.onezone.manage_account import *
from tests.gui.steps.onezone.login_page import *

from tests.gui.steps.oneprovider.data_tab import *
from tests.gui.steps.oneprovider.file_browser import *
from tests.gui.steps.oneprovider.metadata import *

from tests.gui.steps.rest.cdmi import *
from tests.gui.steps.rest.env_up.users import *
from tests.gui.steps.rest.env_up.groups import *
from tests.gui.steps.rest.env_up.spaces import *
from tests.gui.steps.rest.access_tokens import *


from tests.gui.steps.onezone_before_login import *
from tests.gui.steps.onezone_provider_popup import *
from tests.gui.steps.onezone_providers import *

from tests.gui.steps.oneprovider_common import *
from tests.gui.steps.oneprovider_data import *
from tests.gui.steps.oneprovider_spaces import *
from tests.gui.steps.oneprovider_shares import *
from tests.gui.steps.oneprovider_file_list import *
from tests.gui.steps.oneprovider_sidebar_list import *

import pytest
from pytest_bdd import scenarios, scenario


USING_BASE_URL = False
BROWSER = ''
for arg in sys.argv:
    if '--driver=' in arg:
        BROWSER = arg.split('=')[1]
    elif '--base-url' in arg:
        USING_BASE_URL = True

SKIP_REASON_BASE_URL = 'skipping test due to --base-url usage (external environment)'


# @pytest.mark.skipif(USING_BASE_URL, reason=SKIP_REASON_BASE_URL)
# @scenario('../features/onezone/providers.feature',
#           'User sees that when no provider is working appropriate msg is shown')
# def test_user_sees_that_when_no_provider_is_working_appropriate_msg_is_shown():
#     pass
#
#
# # --- FEATURES: all non-destructive (does not change state) ---
scenarios('../features/onezone/access_tokens.feature')
scenarios('../features/onezone/login.feature')
scenarios('../features/onezone/space_creation.feature')
scenarios('../features/onezone/space_management.feature')
scenarios('../features/onezone/spaces_management.feature')
scenarios('../features/onezone/providers.feature')

# scenarios('../features/onezone/alias.feature')

# scenarios('../features/common/account_management.feature')
# scenarios('../features/common/login.feature')
# scenarios('../features/oneprovider_group.feature')
# scenarios('../features/onezone_gui.feature')
# scenarios('../features/oneprovider_data.feature')
# scenarios('../features/oneprovider_space.feature')
# scenarios('../features/oneprovider_2_providers_cdmi.feature')
# scenarios('../features/oneprovider_2_providers.feature')
# scenarios('../features/oneprovider_shares.feature')
# scenarios('../features/oneprovider_metadata.feature')
#
# # limit some tests to chrome (due to multiple files upload can be simulated in selenium only in Chrome)
# if BROWSER == 'Chrome':
#     scenarios('../features/oneprovider_upload_multi_files.feature')
