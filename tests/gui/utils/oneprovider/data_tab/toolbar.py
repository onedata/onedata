"""Utils and fixtures to facilitate operations on toolbar in data tab in oneprovider web GUI.
"""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import WebElement, Button
from tests.gui.utils.generic import rm_css_cls

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataTopToolBar(PageObject):
    create_directory = Button('#create-dir-tool')
    create_file = Button('#create-file-tool')
    share_element = Button('#share-file-tool')
    edit_metadata = Button('#file-metadata-tool')
    upload_file = Button('#upload-file-tool')
    rename_element = Button('#rename-file-tool')
    change_element_permissions = Button('#lock-file-tool')
    copy_element = Button('#copy-file-tool')
    cut_element = Button('#cut-file-tool')
    remove_element = Button('#remove-file-tool')
    show_file_distribution = Button('#file-chunks-tool')

    _upload_input = WebElement('input#toolbar-file-browse')

    def upload_files(self, files):
        """This interaction is very hacky, because uploading files with Selenium
        needs to use input element, but we do not use it directly in frontend.
        So we unhide an input element for a while and pass a local file path to it.
        """
        with rm_css_cls(self.driver, self._upload_input, 'hidden') as elem:
            elem.send_keys(files)
