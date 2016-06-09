"""Utils and fixtures to faciliate operations on Onezone web GUI.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from pytest import fixture


@fixture()
def logout_button(selenium):
    return selenium.find_element_by_css_selector('account-menu a#nav-home.logout')
