"""This module contains suite for features of Onedata GUI
using single browser instance.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")

import sys

import pytest
from pytest_bdd import scenarios, scenario

from tests.gui.steps.rest.cdmi import *
from tests.gui.steps.rest.env_up.users import *
from tests.gui.steps.rest.env_up.groups import *
from tests.gui.steps.rest.env_up.spaces import *

from tests.gui.steps.generic.url import *
from tests.gui.steps.generic.browser_creation import *
from tests.gui.steps.generic.copy_paste import *
from tests.gui.steps.generic.local_file_system import *

from tests.gui.steps.onepanel.account_management import *
from tests.gui.steps.onepanel.login import *
from tests.gui.steps.onepanel.common import *
from tests.gui.steps.onepanel.deployment import *
from tests.gui.steps.onepanel.spaces import *

from tests.gui.steps.onezone.logged_in_common import *
from tests.gui.steps.onezone.user_alias import *
from tests.gui.steps.onezone.access_tokens import *
from tests.gui.steps.onezone.data_space_management import *
from tests.gui.steps.onezone.providers import *
from tests.gui.steps.onezone.manage_account import *
from tests.gui.steps.onezone.login_page import *

from tests.gui.steps.oneprovider.common import *
from tests.gui.steps.oneprovider.data_tab import *
from tests.gui.steps.oneprovider.file_browser import *
from tests.gui.steps.oneprovider.metadata import *
from tests.gui.steps.oneprovider.shares import *
from tests.gui.steps.oneprovider.groups import *
from tests.gui.steps.oneprovider.spaces import *

from tests.gui.steps.common import *
from tests.gui.steps.modal import *
from tests.gui.steps.oneprovider_common import *


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
# --- FEATURES: all non-destructive (does not change state) ---
scenarios('../features/onepanel/account_management.feature')
scenarios('../features/onepanel/login_page.feature')


scenarios('../features/onezone/access_tokens.feature')
scenarios('../features/onezone/login_page.feature')
scenarios('../features/onezone/providers.feature')
scenarios('../features/onezone/user_alias.feature')

scenarios('../features/onezone/space/creation.feature')
scenarios('../features/onezone/space/basic_management.feature')
scenarios('../features/onezone/space/multiple_spaces_management.feature')


scenarios('../features/oneprovider/data/empty_file_browser.feature')
scenarios('../features/oneprovider/data/single_file.feature')
scenarios('../features/oneprovider/data/several_files.feature')
scenarios('../features/oneprovider/data/single_directory.feature')
scenarios('../features/oneprovider/data/nested_directories.feature')
scenarios('../features/oneprovider/data/file_metadata.feature')
scenarios('../features/oneprovider/data/directory_metadata.feature')

scenarios('../features/oneprovider/shares/basic_management.feature')

scenarios('../features/oneprovider/groups/creation.feature')
scenarios('../features/oneprovider/groups/basic_management.feature')

scenarios('../features/oneprovider/spaces/creation.feature')
scenarios('../features/oneprovider/spaces/basic_management.feature')

# # TODO rewrite scenarios to use env up set by rest when it will be possible to have more than 1 provider
# scenarios('../features/oneprovider/multiprovider/cdmi.feature')
# scenarios('../features/oneprovider/multiprovider/basic.feature')

# limit some tests to chrome (due to multiple files upload can be simulated in selenium only in Chrome)
if BROWSER == 'Chrome':
    scenarios('../features/oneprovider/data/upload_multiple_files.feature')
