"""Utils and fixtures to facilitate operation on shares in Oneprovider GUI"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.base import PageObject, ExpandableMixin
from tests.gui.utils.core.web_elements import (Label, WebItemsSequence,
                                               WebItem, Input, Button,
                                               WebElement)
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject

from ..breadcrumbs import Breadcrumbs
from ..file_browser import FileBrowser


class SettingDropdown(PageObject, ExpandableMixin):
    options = WebItemsSequence('ul.dropdown-menu-list li',
                               cls=ButtonWithTextPageObject)
    _toggle = WebElement('.dropdown-toggle[data-toggle="dropdown"]')


class SharesSidebarRecord(PageObject):
    name = id = Label('.item-icon + .item-label')
    settings = WebItem('.settings-dropdown', cls=SettingDropdown)


class SharesSidebar(PageObject):
    title = Label('.secondary-sidebar-header .title')
    shares = WebItemsSequence('ul.shares-list li.first-level',
                              cls=SharesSidebarRecord)


class SharesContentPage(PageObject):
    sidebar = WebItem('.secondary-sidebar', cls=SharesSidebar)
    no_shares_msg = Label('#content-scroll .empty-model-message')
    name = Label('#content-scroll .share-name')
    path = Breadcrumbs('table .file-breadcrumbs-list')
    url = Input('table input')
    copy_url = Button('table button.copy-btn')
    breadcrumbs = Breadcrumbs('.data-files-list .file-breadcrumbs-list')
    file_browser = FileBrowser('.files-list')
