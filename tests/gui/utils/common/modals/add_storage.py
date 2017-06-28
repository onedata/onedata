"""Utils and fixtures to facilitate operations on Add storage modal.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.gui.utils.core.common import PageObject
from tests.gui.utils.core.web_elements import TextLabelWebElement, \
    ButtonWebElement, ButtonWithTextWebElement, WebElement


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

    token = command = TextLabelWebElement('textarea.token-input')
    _copy_btn = ButtonWebElement('.copy-btn')
    _gen_token_btn = ButtonWithTextWebElement('a.clickable',
                                              text='generate another token')

    def generate_another_token(self):
        self._click_on_btn('gen_token')

    def copy(self):
        self._click_on_btn('copy')

    def __str__(self):
        return 'Add storage modal for "{}"'.format(self.title)
