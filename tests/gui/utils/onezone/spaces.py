"""Utils and fixtures to facilitate operations on DATA SPACE MANAGEMENT panel
in Onezone web GUI.
"""

from tests.gui.utils.common.common import ExpandableMixin, PageObject
from tests.gui.utils.common.web_elements import ItemListWebElement, \
    TextLabelWebElement, WebElement, ButtonWebElement, \
    InputWebElement, ToggleWebElement
from tests.gui.utils.generic import find_web_elem_with_text, click_on_web_elem


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class SpaceRecord(PageObject, ExpandableMixin):
    name = TextLabelWebElement('.space-header.truncate',
                               parent_name='given space record')
    size = TextLabelWebElement('.space-header-size')
    providers_count = TextLabelWebElement('.providers-count')
    _toggle = ToggleWebElement('.secondary-item-container .clickable')
    _settings = WebElement('.settings-tool .settings-dropdown')
    _supporting_providers = ItemListWebElement('ul.tertiary-list '
                                               'li.sidebar-space-provide')
    _get_support_btn = ButtonWebElement('ul.tertiary-list '
                                        'li.get-support .dropdown')
    _token = WebElement('ul.tertiary-list li.get-support .dropdown-menu')
    _set_home_btn = ButtonWebElement('.secondary-item-element.star-toggle '
                                     '.oneicon-home-outline')
    _home_space_icon = WebElement('.oneicon-space-home')
    _home_icon = WebElement('.secondary-item-element.star-toggle .oneicon-home')

    def __str__(self):
        return 'space record named: "{}" in {}'.format(self.name,
                                                       str(self._parent))

    def __iter__(self):
        return (ProviderRecord(self._driver, provider, self)
                for provider in self._supporting_providers)

    def __getitem__(self, provider_name):
        for provider in self:
            if provider_name == provider.name:
                return provider
        else:
            raise RuntimeError('no supporting provider named "{prov}" '
                               'for space named "{space}" found in DATA SPACE '
                               'MANAGEMENT oz panel'.format(prov=provider_name,
                                                            space=self.name))

    @property
    def settings(self):
        return SettingsDropdown(self._driver, self._settings, self)

    @property
    def dropright_with_token(self):
        return SpaceSupportTokenDropdownMenu(self._driver, self._token, self)

    def is_home(self):
        try:
            _ = self._home_icon and self._home_space_icon
        except RuntimeError:
            return False
        else:
            return True

    def get_support(self):
        self._click_on_btn('get_support')

    def set_as_home(self):
        if not self.is_home():
            self._click_on_btn('set_home')


class ProviderRecord(PageObject):
    name = TextLabelWebElement('.one-label.truncate',
                               parent_name='given provider record')
    _unsupport_space_btn = ButtonWebElement('.clickable .oneicon-leave-space')
    _click_area = WebElement('.clickable')

    def __str__(self):
        return 'provider record named: "{}" in {}'.format(self.name,
                                                          str(self._parent))

    def click(self):
        err_msg = 'cannot click on provider record named "{}" in submenu of ' \
                  'space in DATA SPACE MANAGEMENT panel'.format(self.name)
        click_on_web_elem(self._driver, self._click_area, err_msg)

    def unsupport_space(self):
        self._click_on_btn('unsupport_space')


class SpaceSupportTokenDropdownMenu(PageObject):
    token = InputWebElement('input')
    _copy_btn = ButtonWebElement('button')

    def __str__(self):
        return 'token dropright in {}'.format(self._parent)

    def copy_token(self):
        self._click_on_btn('copy')


class SettingsDropdown(PageObject, ExpandableMixin):

    def __init__(self, driver, web_elem, *args, **kwargs):
        self._toggle = web_elem
        super(SettingsDropdown, self).__init__(driver, web_elem, *args, **kwargs)

    def __str__(self):
        return 'settings dropdown for {}'.format(str(self._parent))

    def set_as_home(self):
        self._click_on_btn('set as home')

    def rename(self):
        self._click_on_btn('rename')

    def get_support(self):
        self._click_on_btn('get support')

    def leave(self):
        self._click_on_btn('leave')

    def _click_on_btn(self, btn_name):
        btn = self._get_btn(btn_name)
        err_msg = 'cannot click on {btn} in expanded settings for space in ' \
                  'DATA SPACE MANAGEMENT panel disabled'.format(btn=btn_name)
        click_on_web_elem(self._driver, btn, err_msg)

    def _get_btn(self, name):
        css_sel = 'ul.space-dropdown-menu li.clickable'
        err_msg = 'no button named {btn} found in settings ' \
                  'dropdown for given space in DATA SPACE ' \
                  'MANAGAMENT oz panel'.format(btn=name)
        return find_web_elem_with_text(self.web_elem, css_sel,
                                       name, err_msg)
