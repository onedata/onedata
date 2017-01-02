"""Utils and fixtures to facilitate operations on Onezone web GUI.
"""

import re
from pytest import fixture

from selenium.common.exceptions import NoSuchElementException


__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


RE_DATA_URL = re.compile(r'(?P<lang>/.*)?/data/(?P<space>.*)/(?P<dir>.*)')


@fixture()
def logout_button(selenium):
    return selenium.find_element_by_css_selector('account-menu a#nav-home.logout')


class SpaceRecord(object):

    def __init__(self, web_elem):
        self.web_elem = web_elem

    class ProviderRecord(object):
        def __init__(self, web_elem):
            self.web_elem = web_elem

        @property
        def name(self):
            css_sel = '.one-label.truncate'
            return self.web_elem.find_element_by_css_selector(css_sel).text

        def click(self):
            css_sel = '.clickable'
            self.web_elem.find_element_by_css_selector(css_sel).click()

        def unsupport_space(self):
            css_sel = '.clickable .oneicon-leave-space'
            self.web_elem.find_element_by_css_selector(css_sel).click()

    @property
    def providers(self):
        if self.is_submenu_expanded:
            css_sel = 'ul.tertiary-list li.sidebar-space-provide'
            return (SpaceRecord.ProviderRecord(provider) for provider in
                    self.web_elem.find_elements_by_css_selector(css_sel))
        else:
            raise RuntimeError('submenu for space named "{}" '
                               'is not expanded'.format(self.name))

    def __getitem__(self, provider_name):
        for provider in self.providers:
            if provider_name == provider.name:
                return provider

    @property
    def name(self):
        css_sel = '.secondary-item-container .space-header.truncate'
        return self.web_elem.find_element_by_css_selector(css_sel).text

    @property
    def size(self):
        css_sel = '.secondary-item-element.clickable .space-header-size'
        return self.web_elem.find_element_by_css_selector(css_sel).text

    @property
    def providers_count(self):
        css_sel = '.secondary-item-element.providers-count .one-label'
        return int(self.web_elem.find_element_by_css_selector(css_sel).text)

    @property
    def is_home(self):
        css_sel1 = '.secondary-item-element.star-toggle .oneicon-home'
        css_sel2 = '.secondary-item-element.clickable .oneicon-space-home'
        try:
            self.web_elem.find_element_by_css_selector(css_sel1)
            self.web_elem.find_element_by_css_selector(css_sel2)
        except NoSuchElementException:
            return False
        else:
            return True

    def mark_as_home(self):
        if not self.is_home:
            css = '.secondary-item-element.star-toggle .oneicon-home-outline'
            self.web_elem.find_element_by_css_selector(css).click()

    class DropdownMenu(object):
        def __init__(self, web_elem):
            self.web_elem = web_elem

        @property
        def token(self):
            token = self.web_elem.find_element_by_css_selector('input')
            return token.get_attribute('value')

        def copy(self):
            btn = self.web_elem.find_element_by_css_selector('button')
            return btn.click()

    def get_support(self):
        if self.is_submenu_expanded:
            css_sel = 'ul.tertiary-list li.get-support .dropdown'
            btn = self.web_elem.find_element_by_css_selector(css_sel)
            btn.click()
            if 'open' in btn.get_attribute('class'):
                css_sel2 = '{:s} .dropdown-menu'.format(css_sel)
                menu = self.web_elem.find_element_by_css_selector(css_sel2)
                return SpaceRecord.DropdownMenu(menu)
        else:
            raise RuntimeError('submenu for space named "{}" '
                               'is not expanded'.format(self.name))

    @property
    def is_submenu_expanded(self):
        toggle = self._get_secondary_item_container()
        aria_expanded = toggle.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    def expand_submenu(self):
        if not self.is_submenu_expanded:
            self._get_secondary_item_container().click()

    def collapse_submenu(self):
        if self.is_submenu_expanded:
            self._get_secondary_item_container().click()

    def _get_secondary_item_container(self):
        css_sel = '.secondary-item-container .clickable'
        return self.web_elem.find_element_by_css_selector(css_sel)


class OZPanelMixin(object):
    @property
    def is_expanded(self):
        toggle = self._get_main_accordion()
        aria_expanded = toggle.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    def expand(self):
        if not self.is_expanded:
            self._get_main_accordion().click()

    def collapse(self):
        if self.is_expanded:
            self._get_main_accordion().click()

    def _get_main_accordion(self):
        css_sel = 'a.main-accordion-toggle'
        return self.web_elem.find_element_by_css_selector(css_sel)


class DataSpaceManagementPanel(OZPanelMixin):
    def __init__(self, web_elem):
        self.web_elem = web_elem

    def __getitem__(self, item):
        css_sel = '.spaces-accordion-item'
        spaces = (SpaceRecord(space) for space in
                  self.web_elem.find_elements_by_css_selector(css_sel))
        for space in spaces:
            if item == space.name:
                return space


class OZLoggedIn(object):
    def __init__(self, web_elem):
        self.web_elem = web_elem

    def __getattr__(self, item):
        pass
