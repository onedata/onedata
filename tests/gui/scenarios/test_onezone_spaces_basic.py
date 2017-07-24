"""This module contains tests suite for spaces management using
Onezone GUI and single browser instance.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from pytest import fixture
from pytest_bdd import scenario, scenarios

from tests.gui.steps.rest.env_up.users import *
from tests.gui.steps.rest.env_up.groups import *
from tests.gui.steps.rest.env_up.spaces import *

from tests.gui.steps.generic.url import *
from tests.gui.steps.generic.browser_creation import *
from tests.gui.steps.generic.copy_paste import *
from tests.gui.steps.generic.local_file_system import *

from tests.gui.steps.onepanel.account_management import *
from tests.gui.steps.onepanel.login import *
from tests.gui.steps.onepanel.nodes import *
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


@fixture(scope='module')
def screens():
    return [0]


# needed to init environment
@scenario('../features/onepanel/deployment.feature', 'Cluster deployment')
def test_cluster_deployment():
    pass


scenarios('../features/onezone/space/creation.feature')
scenarios('../features/onezone/space/basic_management.feature')
scenarios('../features/onezone/space/multiple_spaces_management.feature')
