"""Utils and fixtures to facilitate operations on panels (base for specific panels)
in Onezone web GUI.
"""

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.mixins import ExpandableMixin
from tests.gui.utils.common.web_elements import ToggleWebElement, \
    TextLabelWebElement, InputWebElement, ButtonWebElement, web_item, \
    ButtonWithTextWebElement, ItemListWebElement, WebElement
from tests.gui.utils.generic import suppress, click_on_web_elem
from tests.gui.utils.onezone.edit_box import EditBox
from tests.gui.utils.onezone.providers import ProviderRecord

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@web_item
class OZPanel(PageObject, ExpandableMixin):
    name = TextLabelWebElement('a.main-accordion-toggle',
                               parent_name='oz panel')
    _toggle = ToggleWebElement('a.main-accordion-toggle')

    def __str__(self):
        return '{} panel in {}'.format(self.name, str(self._parent))


@web_item
class UserAliasPanel(OZPanel):
    alias = TextLabelWebElement('.alias-text')
    _edit_area = WebElement('.alias-accordion-toggle.clickable')
    _rename_btn = ButtonWebElement('.oneicon-rename')

    def edit_alias(self):

        with suppress(NoSuchElementException):
            elem = self._rename_btn
            err_msg = 'clicking on rename btn in {} disabled'.format(str(self))
            click_on_web_elem(self._driver, elem, err_msg)

        return EditBox(self._driver, self._edit_area, self)


@web_item
class AccessTokensPanel(OZPanel):
    _create_token = ButtonWithTextWebElement('.clickable',
                                             text='create new access token')
    _tokens = ItemListWebElement('.tokens-list-item')

    @property
    def tokens_count(self):
        return len(self._tokens)

    def __iter__(self):
        return (TokenRecord(token, self._driver)
                for token in self._tokens)

    def __getitem__(self, idx):
        tokens = self._tokens
        try:
            return TokenRecord(self._driver, tokens[idx], self)
        except IndexError:
            raise RuntimeError('asked for number {idx} token but there are '
                               'only {num} tokens in {item}'
                               ''.format(idx=idx, num=len(tokens),
                                         item=str(self)))

    def create_new_access_token(self):
        err_msg = 'clicking on Create new space in {} disabled'.format(str(self))
        click_on_web_elem(self._driver, self._create_token, err_msg)


class GoToYourFilesPanel(OZPanel):
    _providers = ItemListWebElement('#providers-list .providers-accordion-item')

    def __iter__(self):
        return (ProviderRecord(provider, self._driver, 'provider', '.spaces-count')
                for provider in self._providers)

    def __getitem__(self, name):
        for provider in self.providers:
            if name == provider.name:
                return provider
        else:
            raise RuntimeError('no provider named "{prov}" found in '
                               '{parent}'.format(prov=name, parent=str(self)))


@web_item
class TokenRecord(PageObject):
    value = InputWebElement('.token-header input')
    _copy_btn = ButtonWebElement('.oneicon-clipboard-copy')
    _remove_btn = ButtonWebElement('.oneicon-remove')

    def __str__(self):
        return 'token record in {}'.format(self._parent)

    def copy(self):
        err_msg = 'clicking on cp btn in {} disabled'.format(str(self))
        click_on_web_elem(self._driver, self._copy_btn, err_msg)

    def remove(self):
        err_msg = 'clicking on rm btn in {} disabled'.format(str(self))
        click_on_web_elem(self._driver, self._remove_btn, err_msg)
