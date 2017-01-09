"""Utils and fixtures to facilitate operations on Onezone web GUI.
"""

import re

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.generic import find_web_elem, find_web_elem_with_text
from tests.gui.utils.onezone.access_tokens import AccessTokensPanel
from tests.gui.utils.onezone.data_space_management import DataSpaceManagementPanel
from tests.gui.utils.onezone.sidebar_panel import OZPanel
from tests.gui.utils.onezone.sidebar_panel_record import OZPanelRecord
from tests.gui.utils.onezone.user_alias import UserAliasPanel

__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


RE_DATA_URL = re.compile(r'(?P<lang>/.*)?/data/(?P<space>.*)/(?P<dir>.*)')


class SpaceRecordInProvidersPanel(object):
    def __init__(self, web_elem, name_css, size_css):
        self.web_elem = web_elem
        self.name_css = name_css
        self.size_css = size_css

    @property
    def name(self):
        err_msg = 'unable to locate name header for supported space in ' \
                  'displayed provider panel'
        header = find_web_elem(self.web_elem, self.name_css, err_msg)
        return header.text

    @property
    def size(self):
        err_msg = 'no size label for space named "{}" found ' \
                  'in displayed provider panel'.format(self.name)
        size_label = find_web_elem(self.web_elem, self.size_css, err_msg)
        return size_label.text

    @property
    def is_home(self):
        css_sel = '.oneicon-space-default'
        try:
            self.web_elem.find_element_by_css_selector(css_sel)
        except NoSuchElementException:
            return False
        else:
            return True


class ProviderRecord(OZPanelRecord):

    @property
    def is_working(self):
        css_sel = '.provider-icon .color-provider-working'
        try:
            self.web_elem.find_element_by_css_selector(css_sel)
        except NoSuchElementException:
            return False
        else:
            return True

    @property
    def spaces_count(self):
        css_sel = '.spaces-count'
        err_msg = 'no spaces count for "{}" provider found in ' \
                  'GO TO YOUR FILES panel'.format(self.name)
        spaces_count = find_web_elem(self.web_elem, css_sel, err_msg)
        return int(spaces_count)

    def click_on(self):
        css_sel = '.secondary-item-container'
        err_msg = 'no provider header for "{}" found in GO TO YOUR FILES ' \
                  'panel'.format(self.name)
        header = find_web_elem(self.web_elem, css_sel, err_msg)
        header.click()

    def unset_from_home(self):
        css_sel = '.secondary-item-element.star-toggle .oneicon-home'
        err_msg = 'no home icon found for "{}" provider in GO TO YOUR FILES ' \
                  'panel'.format(self.name)
        home_icon = find_web_elem(self.web_elem, css_sel, err_msg)
        home_icon.click()

    @property
    def supported_spaces(self):
        if self.is_submenu_expanded:
            css_sel = 'ul.tertiary-list li.sidebar-provider-space'
            return (SpaceRecordInProvidersPanel(space,
                                                name_css='.one-label.truncate',
                                                size_css='.space-header-size')
                    for space
                    in self.web_elem.find_elements_by_css_selector(css_sel))
        else:
            raise RuntimeError('submenu for provider named "{}" '
                               'is not expanded in GO TO YOUR FILES '
                               'panel'.format(self.name))

    def __getitem__(self, space_name):
        for space in self.supported_spaces:
            if space_name == space.name:
                return space
        else:
            raise RuntimeError('no supported space named "{space}" '
                               'for provider named "{provider}" in '
                               'GO TO YOUR FILES panel '
                               'found'.format(provider=self.name,
                                              space=space_name))


class GoToYourFilesPanel(OZPanel):

    @property
    def providers(self):
        css_sel = '#providers-list .providers-accordion-item'
        return (ProviderRecord(provider, 'provider', '.spaces-count')
                for provider
                in self.web_elem.find_elements_by_css_selector(css_sel))

    def __getitem__(self, name):
        for provider in self.providers:
            if name == provider.name:
                return provider
        else:
            raise RuntimeError('no provider named "{prov}" found in {panel} '
                               'oz panel'.format(prov=name, panel=self.name))


class ManageAccount(object):

    def __init__(self, web_elem):
        self.web_elem = web_elem

    class AccountDropdown(object):
        def __init__(self, web_elem):
            self.web_elem = web_elem

        def logout(self):
            btn = self._get_btn('logout')
            btn.click()

        def _get_btn(self, name):
            css_sel = 'li a'
            err_msg = 'no button named {btn} found in account dropdown ' \
                      'for MANAGE ACCOUNT in oz panel'.format(btn=name)
            return find_web_elem_with_text(self.web_elem, css_sel,
                                           name, err_msg)

    @property
    def account_dropdown(self):
        if self.is_account_dropdown_expanded:
            css_sel = 'ul.dropdown-menu-list'
            err_msg = 'no account dopdown found in MANAGE ACCOUNT in oz panel'
            return ManageAccount.AccountDropdown(find_web_elem(self.web_elem,
                                                               css_sel, err_msg))
        else:
            raise RuntimeError('account dropdown in MANAGE ACCOUNT '
                               'in oz is not expanded')

    @property
    def is_account_dropdown_expanded(self):
        toggle = self._get_account_dropdown_toggle()
        return self._is_account_dropdown_expanded(toggle)

    def expand_account_dropdown(self):
        toggle = self._get_account_dropdown_toggle()
        if not self._is_account_dropdown_expanded(toggle):
            toggle.click()

    def collapse_account_dropdown(self):
        toggle = self._get_account_dropdown_toggle()
        if self._is_account_dropdown_expanded(toggle):
            toggle.click()

    def _is_account_dropdown_expanded(self, toggle):
        aria_expanded = toggle.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    def _get_account_dropdown_toggle(self):
        css_sel = 'li.account-menu a.dropdown-toggle'
        err_msg = 'no account dropdown toggle found in MANAGE ACCOUNT in oz panel'
        return find_web_elem(self.web_elem, css_sel, err_msg)


class WorldMap(object):

    def __init__(self, web_elem):
        self.web_elem = web_elem

    @property
    def providers(self):
        css_sel = '.provider-place'
        return (ProviderDropPanel(provider) for provider in
                self.web_elem.find_elements_by_css_selector(css_sel))

    def __getitem__(self, index):
        i = 0
        for i, provider in enumerate(self.providers):
            if i == index:
                return provider
        else:
            raise RuntimeError('asked for {index} provider but there are only '
                               '{num} providers on map'.format(index=index,
                                                               num=i))

    def get_provider_with_displayed_panel(self):
        for provider in self.providers:
            if provider.is_displayed:
                return provider


class OZLoggedIn(object):
    panels = {'data space management': DataSpaceManagementPanel,
              'go to your files': GoToYourFilesPanel,
              'access tokens': AccessTokensPanel,
              'user alias': UserAliasPanel}

    def __init__(self, web_elem):
        self.web_elem = web_elem

    def __getitem__(self, panel):
        panel = panel.lower()
        cls = self.panels.get(panel, None)
        if cls:
            panel = panel.replace('_', ' ').lower()
            css_sel = '.main-accordion-group, a.main-accordion-toggle'
            items = self.web_elem.find_elements_by_css_selector(css_sel)
            for group, toggle in zip(items[::2], items[1::2]):
                if panel == toggle.text.lower():
                    return cls(group)

        elif panel == 'manage account':
            css_sel = 'header.onezone-top-bar'
            err_msg = 'no header for oz page found'
            header = find_web_elem(self.web_elem, css_sel, err_msg)
            return ManageAccount(header)

        elif panel == 'world map':
            css_sel = '.onezone-atlas'
            err_msg = 'no world map found on oz page'
            world_map = find_web_elem(self.web_elem, css_sel, err_msg)
            return WorldMap(world_map)


class ProviderDropPanel(object):

    def __init__(self, web_elem):
        self.web_elem = web_elem

    def click_on(self):
        self.web_elem.click()

    @property
    def is_working(self):
        return 'working' in self.web_elem.get_attribute('class')

    @property
    def is_displayed(self):
        css_sel = '.provider-place-drop'
        try:
            self.web_elem.find_element_by_css_selector(css_sel)
        except NoSuchElementException:
            return False
        else:
            return True

    @property
    def name(self):
        if self.is_displayed:
            css_sel = '.title-label'
            err_msg = 'no name found in displayed provider drop panel'
            header = find_web_elem(self.web_elem, css_sel, err_msg)
            return header.text
        else:
            raise RuntimeError('no displayed panel found for given provider')

    @property
    def hostname(self):
        if self.is_displayed:
            css_sel = 'input.provider-host-text'
            err_msg = 'no hostname found in displayed provider drop panel'
            header = find_web_elem(self.web_elem, css_sel, err_msg)
            return header.get_attribute('value')
        else:
            raise RuntimeError('no displayed panel found for given provider')

    def copy_hostname(self):
        if self.is_displayed:
            css_sel = '.provider-host-copy-btn'
            err_msg = 'no copy hostname btn found in displayed provider drop panel'
            btn = find_web_elem(self.web_elem, css_sel, err_msg)
            btn.click()
        else:
            raise RuntimeError('no displayed panel found for given provider')

    def go_to_your_files(self):
        if self.is_displayed:
            css_sel = '.drop-body button'
            err_msg = "no 'Go to your files' btn found " \
                      "in displayed provider drop panel"
            btn = find_web_elem(self.web_elem, css_sel, err_msg)
            btn.click()
        else:
            raise RuntimeError('no displayed panel found for given provider')

    @property
    def supported_spaces(self):
        if self.is_displayed:
            css_sel = 'ul li.provider-place-drop-space'
            return (SpaceRecordInProvidersPanel(space,
                                                name_css='.space-label',
                                                size_css='.space-size')
                    for space
                    in self.web_elem.find_elements_by_css_selector(css_sel))
        else:
            raise RuntimeError('no displayed panel found for given provider')

    def __getitem__(self, space_name):
        if self.is_displayed:
            for space in self.supported_spaces:
                if space_name == space.name:
                    return space
            else:
                raise RuntimeError('no supported space named "{space}" '
                                   'for provider named "{provider}" in displayed '
                                   'drop panel found'.format(provider=self.name,
                                                             space=space_name))
        else:
            raise RuntimeError('no displayed panel found for given provider')
