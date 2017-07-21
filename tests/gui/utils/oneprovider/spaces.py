"""Utils and fixtures to facilitate operation on spaces in Oneprovider GUI"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import (Label, WebItemsSequence,
                                               WebItem, Button, WebElement)
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject


class SettingDropdown(PageObject, ExpandableMixin):
    options = WebItemsSequence('ul.dropdown-menu-list li.clickable',
                               cls=ButtonWithTextPageObject)
    _toggle = WebElement('.dropdown-toggle[data-toggle="dropdown"]')


class SpaceSidebarRecord(PageObject):
    name = id = Label('.item-icon + .item-label')
    settings = WebItem('.settings-dropdown', cls=SettingDropdown)
    users = Button('ul li.users-permissions .item-click-area')
    groups = Button('ul li.groups-permissions .item-click-area')
    _space_icon = WebElement('.item-icon [class*="oneicon-space"]')

    def is_selected(self):
        return 'active' in self.web_elem.get_attribute('class')

    def is_home(self):
        return 'oneicon-space-home' in self._space_icon.get_attribute('class')


class SpacesSidebar(PageObject):
    title = Label('.secondary-sidebar-header .title')
    create = Button('.oneicon-space-add')
    join = Button('.oneicon-join')
    spaces = WebItemsSequence('ul.spaces-list li.first-level',
                              cls=SpaceSidebarRecord)


class PermissionTableRow(PageObject):
    name = id = Label('.one-first .truncated-string-content')


class PermissionTable(PageObject):
    users = groups = WebItemsSequence('tbody .permissions-table-row',
                                      cls=PermissionTableRow)
    save = Button('button.btn-save')
    discard = Button('button.btn-discard')


class SpacesContentPage(PageObject):
    sidebar = WebItem('.secondary-sidebar', cls=SpacesSidebar)
    permission_table = WebItem('#content-scroll .permissions-table',
                               cls=PermissionTable)
