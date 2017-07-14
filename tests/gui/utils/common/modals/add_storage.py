"""Utils and fixtures to facilitate operations on Add storage modal.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (Label, Button, NamedButton,
                                               WebElement)


class AddStorage(PageObject):
    _header = WebElement('.modal-header')
    _space_name = WebElement('.modal-header .special-name')

    @property
    def title(self):
        header = self.driver.execute_script(
            '$(arguments[0]).clone().children().remove().end().text()',
            self._header
        )
        return (header + ' ' + self._space_name).lower()

    token = command = Label('textarea.token-input')
    copy = Button('.copy-btn')
    generate_token = NamedButton('a.clickable', text='generate another token')

    def __str__(self):
        return 'Add storage modal for "{}"'.format(self.title)
