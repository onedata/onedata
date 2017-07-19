"""Utils and fixtures to facilitate operations on toolbar in data tab in oneprovider web GUI.
"""

from tests.gui.utils.core.common import PageObject
from tests.gui.utils.core.web_elements import WebElement, ButtonWebItem
from tests.gui.utils.generic import rm_css_cls

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataTopToolBar(PageObject):
    create_directory = ButtonWebItem('li a#create-dir-tool')
    create_file = ButtonWebItem('li a#create-file-tool')
    share_element = ButtonWebItem('li a#share-file-tool')
    edit_metadata = ButtonWebItem('li a#file-metadata-tool')
    upload_file = ButtonWebItem('li a#upload-file-tool')
    rename_element = ButtonWebItem('li a#rename-file-tool')
    change_element_permissions = ButtonWebItem('li a#lock-file-tool')
    copy_element = ButtonWebItem('li a#copy-file-tool')
    cut_element = ButtonWebItem('li a#cut-file-tool')
    remove_element = ButtonWebItem('li a#remove-file-tool')
    show_file_distribution = ButtonWebItem('li a#file-chunks-tool')

    _upload_input = WebElement('input#toolbar-file-browse')

    def upload_files(self, files):
        """This interaction is very hacky, because uploading files with Selenium
        needs to use input element, but we do not use it directly in frontend.
        So we unhide an input element for a while and pass a local file path to it.
        """
        with rm_css_cls(self.driver, self._upload_input, 'hidden') as elem:
            elem.send_keys(files)

    def __str__(self):
        return 'toolbar in {}'.format(self.parent)
