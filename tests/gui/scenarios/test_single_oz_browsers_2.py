"""Test suite for features of Onezone login page.
"""
__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.steps.common import *
from tests.gui.steps.modal import *


from tests.gui.steps.generic.url import *
from tests.gui.steps.generic.browser_creation import *
from tests.gui.steps.generic.copy_paste import *
from tests.gui.steps.generic.login import *

from tests.gui.steps.onepanel.common import *
from tests.gui.steps.onepanel.deployment import *
from tests.gui.steps.onepanel.space_support import *
from tests.gui.steps.onepanel.nodes import *
from tests.gui.steps.onepanel.storages import *

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

from tests.gui.steps.oneservices.cdmi import *


from tests.gui.steps.onezone_before_login import *
from tests.gui.steps.onezone_provider_popup import *
from tests.gui.steps.onezone_providers import *

from tests.gui.steps.oneprovider_common import *
from tests.gui.steps.oneprovider_data import *
from tests.gui.steps.oneprovider_spaces import *
from tests.gui.steps.oneprovider_shares import *
from tests.gui.steps.oneprovider_file_list import *
from tests.gui.steps.oneprovider_sidebar_list import *

from pytest_bdd import scenarios, scenario


# --- FEATURES: all non-destructive (does not change state) ---
# scenarios('../features/onepanel/provider.feature')

scenarios('../features/onepanel/storages.feature')
scenarios('../features/onepanel/deployment.feature')
scenarios('../features/onepanel/spaces.feature')

# scenarios('../features/oneprovider_2_providers_multi.feature')
# scenarios('../features/oneprovider_shares_multi.feature')
# scenarios('../features/oneprovider_group_multi.feature')
# scenarios('../features/onezone_gui_multi.feature')
