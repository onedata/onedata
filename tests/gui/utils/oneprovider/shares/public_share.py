"""Utils and fixtures to facilitate operation on public share view"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.web_elements import Label, Button, Input
from ..breadcrumbs import Breadcrumbs
from ..file_browser import FileBrowser


class PublicShareView(object):
    name = Label('.share-name')
    url = Input('.share-info-head input')
    copy_url = Button('.share-info-head button.copy-btn')
    breadcrumbs = Breadcrumbs('.file-breadcrumbs')
    file_browser = FileBrowser('.files-list')

    def __init__(self, driver):
        self.web_elem = self.driver = driver

    def __str__(self):
        return 'Public share view'
