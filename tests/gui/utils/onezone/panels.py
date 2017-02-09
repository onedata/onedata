"""Utils and fixtures to facilitate operations on panels (base for specific panels)
in Onezone web GUI.
"""

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.common.common import PageObject, ExpandableMixin
from tests.gui.utils.common.web_elements import ToggleWebElement, \
    TextLabelWebElement, ButtonWebElement, \
    ButtonWithTextWebElement, ItemListWebElement, WebElement
from tests.gui.utils.generic import suppress, click_on_web_elem, iter_ahead
from tests.gui.utils.onezone.edit_box import EditBox
from tests.gui.utils.onezone.providers import ProviderRecord
from tests.gui.utils.onezone.tokens import TokenRecord
from tests.gui.utils.onezone.spaces import SpaceRecord

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class OZPanel(PageObject, ExpandableMixin):
    name = TextLabelWebElement('a.main-accordion-toggle',
                               parent_name='oz panel')
    _toggle = ToggleWebElement('a.main-accordion-toggle')

    def __str__(self):
        return '{} panel in {}'.format(self.name, str(self._parent))


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


class AccessTokensPanel(OZPanel):
    _create_token_btn = ButtonWithTextWebElement('.clickable',
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
        self._click_on_btn('create_token')


class GoToYourFilesPanel(OZPanel):
    _providers = ItemListWebElement('#providers-list .providers-accordion-item')

    def __iter__(self):
        return (ProviderRecord(self._driver, provider, self)
                for provider in self._providers)

    def __getitem__(self, name):
        for provider in self:
            if name == provider.name:
                return provider
        else:
            raise RuntimeError('no provider named "{prov}" found in '
                               '{parent}'.format(prov=name, parent=str(self)))


class DataSpaceManagementPanel(OZPanel):
    _spaces = ItemListWebElement('.spaces-accordion-item')
    _join_space_btn = ButtonWithTextWebElement('.clickable', text='join a space')
    _create_space_btn = ButtonWithTextWebElement('.clickable',
                                                 text='create new space')

    def __str__(self):
        return ''

    def __iter__(self):
        return (SpaceRecord(self._driver, space, self)
                for space in self._spaces)

    def __getitem__(self, name):
        for space in self:
            if name == space.name:
                return space
        else:
            raise RuntimeError('no space named "{space}" found in '
                               '{item}'.format(space=name, item=str(self)))

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
                return EditBox(self._driver, item, self)
        else:
            raise RuntimeError('no edit box for create new space found '
                               'in "{panel}" oz panel'.format(panel=self.name))
