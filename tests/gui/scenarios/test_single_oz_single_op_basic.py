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

# --- FEATURES: all non-destructive (does not change state) ---
scenarios('../features/onezone_login.feature')
scenarios('../features/onezone_gui.feature')


# --- FEATURES: oneprovider_data --- #

@pytest.mark.xfail(reason='Fails on Chrome due to VFS-2189', run=False)
@pytest.mark.destructive
@scenario('../features/oneprovider_data.feature',
          'After failed upload to space that rejects the file, the same file can be successfully uploaded to other space that accepts the file')
def test_upload_fail_and_then_success_one_file():
    pass


@pytest.mark.xfail(reason='Fails on Chrome due to VFS-2189', run=False)
@pytest.mark.destructive
@scenario('../features/oneprovider_data.feature',
          'After failed upload some file to broken space, an other file can be successfully uploaded to correct space')
def test_upload_fail_and_then_success_other_files():
    pass


@pytest.mark.xfail(reason='Fails randomly, need to find out what is a problem', run=False)
@pytest.mark.destructive
@scenario('../features/oneprovider_data.feature',
          'Uploading a file whose size exceeds the space quota should fail')
def test_upload_too_big_file():
    pass


@pytest.mark.xfail(reason='Fails randomly, need to find out what is a problem', run=False)
@pytest.mark.destructive
@scenario('../features/oneprovider_data.feature',
          'Uploading a file to space should succeed')
def test_upload_success():
    pass

