"""This module contains tests suite for basic operations using
Oneprovider data tab GUI and single browser instance.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from functools import partial

from pytest import fixture, mark
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

from . import BROWSER


@fixture(scope='module')
def screens():
    return [0]


# needed to init environment
@scenario('../features/onepanel/deployment.feature', 'Cluster deployment')
def test_cluster_deployment():
    pass


scenario = partial(scenario, '../features/oneprovider/data/upload_multiple_files.feature')

skip_if_not_chrome = mark.skipif(BROWSER != 'Chrome',
                                 reason='some behaviour like multiple file '
                                        'upload at once can only be '
                                        'simulated in Chrome')


@skip_if_not_chrome
@scenario('User uploads 5 files at once')
def test_user_uploads_5_files_at_once():
    pass


@skip_if_not_chrome
@scenario('User uploads more than 50 files and uses files list lazy loading')
def test_user_uploads_more_than_50_files_and_uses_files_list_lazy_loading():
    pass


@skip_if_not_chrome
@scenario('User changes directory while uploading bunch of files')
def test_user_changes_directory_while_uploading_bunch_of_files():
    pass


@skip_if_not_chrome
@scenario('User uploads files and sees their ordering (uploads a bunch of files at once)')
def test_user_uploads_files_and_sees_their_ordering():
    pass


scenarios('../features/oneprovider/data/empty_file_browser.feature')
scenarios('../features/oneprovider/data/single_file.feature')
scenarios('../features/oneprovider/data/several_files.feature')
scenarios('../features/oneprovider/data/single_directory.feature')
scenarios('../features/oneprovider/data/nested_directories.feature')
