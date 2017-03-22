"""Utils and fixtures to facilitate operations on DATA SPACE MANAGEMENT panel
in Onezone web GUI.
"""

from tests.gui.utils.core.common import ExpandableMixin, PageObject
from tests.gui.utils.core.web_elements import TextLabelWebElement, WebElement, ButtonWebElement, \
    InputWebElement, ToggleWebElement, WebItemsSequence, ButtonWithTextWebElement, WebItem
from tests.gui.utils.generic import find_web_elem_with_text, click_on_web_elem, iter_ahead
from .common import OZPanel, EditBox

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _ProviderRecord(PageObject):
    name = id = TextLabelWebElement('.one-label.truncate',
                                    parent_name='given provider record')
    _unsupport_space_btn = ButtonWebElement('.clickable .oneicon-leave-space')
    _click_area = WebElement('.clickable')

    def __str__(self):
        return 'provider record named: "{}" in {}'.format(self.name, self.parent)

    def unsupport_space(self):
        self._click_on_btn('unsupport_space')


class _TokenDropdownMenu(PageObject):
    token = InputWebElement('input')
    _copy_btn = ButtonWebElement('button')

    def __str__(self):
        return 'token dropright in {}'.format(self.parent)

    def copy_token(self):
        self._click_on_btn('copy')


class _SettingsDropdown(PageObject, ExpandableMixin):

    def __init__(self, driver, web_elem, *args, **kwargs):
        self._toggle = web_elem
        super(_SettingsDropdown, self).__init__(driver, web_elem, *args, **kwargs)

    def __str__(self):
        return 'settings dropdown for {}'.format(self.parent)

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
        click_on_web_elem(self.driver, btn,
                          lambda: 'cannot click on {btn} in '
                                  '{item}'.format(btn=btn_name, item=self))

    def _get_btn(self, name):
        css_sel = 'ul.space-dropdown-menu li.clickable'
        return find_web_elem_with_text(self.web_elem, css_sel, name,
                                       'no button named {btn} found in '
                                       '{item}'.format(btn=name, item=self))


class _SpaceRecord(PageObject, ExpandableMixin):
    name = id = TextLabelWebElement('.space-header.truncate',
                                    parent_name='given space record')
    size = TextLabelWebElement('.space-header-size')
    providers_count = TextLabelWebElement('.providers-count')
    settings = WebItem('.settings-tool .settings-dropdown', cls=_SettingsDropdown)
    providers = WebItemsSequence('ul.tertiary-list li.sidebar-space-provide',
                                 cls=_ProviderRecord)
    dropright_with_token = WebItem('ul.tertiary-list li.get-support '
                                   '.dropdown-menu', cls=_TokenDropdownMenu)
    _toggle = ToggleWebElement('.secondary-item-container .clickable')
    _get_support_btn = ButtonWebElement('ul.tertiary-list '
                                        'li.get-support .dropdown')
    _set_home_btn = ButtonWebElement('.secondary-item-element.star-toggle '
                                     '.oneicon-home-outline')
    _home_space_icon = WebElement('.oneicon-space-home')
    _home_icon = WebElement('.secondary-item-element.star-toggle .oneicon-home')

    def __str__(self):
        return 'space record named: "{}" in {}'.format(self.name, self.parent)

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


class DataSpaceManagementPanel(OZPanel):
    spaces = WebItemsSequence('.spaces-accordion-item', cls=_SpaceRecord)
    _join_space_btn = ButtonWithTextWebElement('.clickable', text='join a space')
    _create_space_btn = ButtonWithTextWebElement('.clickable',
                                                 text='create new space')

    def join_space(self):
        self._click_on_btn('join_space')

    def create_new_space(self):
        self._click_on_btn('create_space')

    @property
    def create_space_edit_box(self):
        css_sel = '.clickable, .clickable input[id=create-new-space-name]'
        items = self.web_elem.find_elements_by_css_selector(css_sel)
        for item, next_item in iter_ahead(items):
            if next_item.tag_name == 'input':
                return EditBox(self.driver, item, self)
        else:
            raise RuntimeError('no edit box for create new space found '
                               'in {item}'.format(item=self))
