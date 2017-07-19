"""Test suite for features of Onezone login page.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.steps.rest.cdmi import *
from tests.gui.steps.rest.env_up.users import *
from tests.gui.steps.rest.env_up.groups import *
from tests.gui.steps.rest.env_up.spaces import *

from tests.gui.steps.generic.url import *
from tests.gui.steps.generic.browser_creation import *
from tests.gui.steps.generic.copy_paste import *
from tests.gui.steps.generic.docker import *
from tests.gui.steps.generic.local_file_system import *

from tests.gui.steps.onepanel.login import *
from tests.gui.steps.onepanel.common import *
from tests.gui.steps.onepanel.deployment import *
from tests.gui.steps.onepanel.spaces import *
from tests.gui.steps.onepanel.nodes import *
from tests.gui.steps.onepanel.storages import *
from tests.gui.steps.onepanel.provider import *

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


from tests.gui.steps.oneprovider_common import *
from tests.gui.steps.oneprovider_spaces import *
from tests.gui.steps.oneprovider_sidebar_list import *

from tests.gui.steps.common import *
from tests.gui.steps.modal import *


from pytest_bdd import scenarios, scenario


# --- FEATURES: all non-destructive (does not change state) ---
scenarios('../features/onepanel/deployment.feature')
# scenarios('../features/onepanel/provider.feature')
# scenarios('../features/onepanel/storages.feature')
# scenarios('../features/onepanel/spaces.feature')
# scenarios('../features/onepanel/storage_sync.feature')

scenarios('../features/onezone/space_join_methods.feature')

# scenarios('../features/oneprovider_shares_multi.feature')
# scenarios('../features/oneprovider_group_multi.feature')

# # TODO rewrite scenarios to use env up set by rest when it will be possible to have more than 1 provider
# scenarios('../features/oneprovider/multiprovider/multi_browser.feature')
