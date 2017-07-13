"""Utils and fixtures to facilitate operations on DATA SPACE MANAGEMENT panel
in Onezone web GUI.
"""

from tests.gui.utils.core.base import ExpandableMixin, PageObject
from tests.gui.utils.core.web_elements import Label, WebElement, Button, Input, \
    WebItemsSequence, NamedButton, WebItem
from tests.gui.utils.generic import find_web_elem_with_text, click_on_web_elem
from .common import OZPanel, EditBox

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _ProviderRecord(PageObject):
    name = id = Label('.one-label.truncate',
                      parent_name='given provider record')
    unsupport_space = Button('.clickable .oneicon-leave-space')
    _click_area = WebElement('.clickable')

    def __str__(self):
        return '{} provider record in {}'.format(self.name, self.parent)


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

    def add_storage(self):
        self._click_on_btn('add storage')

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
    name = id = Label('.space-header.truncate',
                      parent_name='given space record')
    size = Label('.space-header-size')
    set_as_home = Button('.secondary-item-element.star-toggle '
                         '.oneicon-home-outline')
    unset_from_home = Button('.secondary-item-element.star-toggle '
                             '.oneicon-home')
    providers_count = Label('.providers-count')
    settings = WebItem('.settings-tool .settings-dropdown', cls=_SettingsDropdown)
    providers = WebItemsSequence('ul.tertiary-list li.sidebar-space-provide',
                                 cls=_ProviderRecord)
    add_storage = Button('ul.tertiary-list li.get-support')
    _toggle = WebElement('.secondary-item-container .clickable')
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


class DataSpaceManagementPanel(OZPanel):
    spaces = WebItemsSequence('.spaces-accordion-item', cls=_SpaceRecord)
    join_space = NamedButton('.clickable', text='join a space')
    create_new_space = NamedButton('.clickable', text='create new space')
    create_space_edit_box = WebItem('.spaces-accordion-toggle.create-new'
                                    '.clickable', cls=EditBox)
