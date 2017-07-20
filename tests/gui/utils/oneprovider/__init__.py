"""Utils for Oneprovider GUI tests
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.web_elements import WebItem, Label
from tests.gui.utils.generic import find_web_elem_with_text, click_on_web_elem
from .data_tab import DataTab
from .user_profile import UserProfile
from .shares import SharesContentPage
from .groups import GroupContentPage
from .spaces import SpacesContentPage


class OPLoggedIn(object):
    provider_name = Label('header .navbar-header .provider-name')
    user_profile = WebItem('li.profile-dropdown', cls=UserProfile)

    tabs = {'data': DataTab,
            'shares': SharesContentPage,
            'groups': GroupContentPage,
            'spaces': SpacesContentPage}

    def __init__(self, driver):
        self.web_elem = self.driver = driver

    def __str__(self):
        return 'Oneprovider page'

    def __getattr__(self, item):
        return self.tabs[item](self.web_elem, self.web_elem, self)

    def click_on_tab(self, name):
        css_sel = 'nav.primary-sidebar li a'
        err_msg = '{} btn in {} not found'.format(name, self)
        btn = find_web_elem_with_text(self.web_elem, css_sel,
                                      name.lower(), err_msg)

        err_msg = 'cannot click on {} in {}'.format(name, self)
        click_on_web_elem(self.web_elem, btn, err_msg)
