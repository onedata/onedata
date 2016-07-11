"""Utils and fixtures to facilitate operations on Onezone web GUI.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import re
from pytest import fixture


RE_DATA_URL = re.compile(r'(?P<lang>/.*)?/data/(?P<space>.*)/(?P<dir>.*)')


@fixture()
def logout_button(selenium):
    return selenium.find_element_by_css_selector('account-menu a#nav-home.logout')
