"""Utils and fixtures to facilitate operations on ACCESS TOKENS panel in Onezone web GUI.
"""

from tests.gui.utils.generic import find_web_elem, click_on_web_elem
from tests.gui.utils.onezone.sidebar_panel import OZPanel


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class AccessTokensPanel(OZPanel):

    @property
    def tokens(self):
        return [TokenRecord(token, self._driver) for token in
                self._get_tokens_web_elements()]

    @property
    def tokens_count(self):
        return len(self._get_tokens_web_elements())

    def __getitem__(self, index):
        tokens = self._get_tokens_web_elements()
        try:
            return TokenRecord(tokens[index], self._driver)
        except IndexError:
            raise RuntimeError('asked for number {index} token but there are '
                               'only {num} tokens in ACCESS TOKENS oz panel'
                               ''.format(index=index, num=len(tokens)))

    def create_new_access_token(self):
        btn = self._get_btn('create new access token')
        err_msg = 'clicking on Create new space in ACCESS TOKENS panel disabled'
        click_on_web_elem(self._driver, btn, err_msg)

    def _get_tokens_web_elements(self):
        css_sel = '.tokens-list-item'
        return self.web_elem.find_elements_by_css_selector(css_sel)


class TokenRecord(object):

    def __init__(self, web_elem, driver):
        self.web_elem = web_elem
        self._driver = driver

    @property
    def value(self):
        css_sel = '.token-header input'
        err_msg = 'no value found for given token in ACCESS TOKEN oz panel'
        header = find_web_elem(self.web_elem, css_sel, err_msg)
        return header.get_attribute('value')

    def copy(self):
        css_sel = '.oneicon-clipboard-copy'
        err_msg = 'no copy btn found for given token in ACCESS TOKEN oz panel'
        btn = find_web_elem(self.web_elem, css_sel, err_msg)
        err_msg = 'clicking on cp btn in access token dropright disabled'
        click_on_web_elem(self._driver, btn, err_msg)

    def remove(self):
        css_sel = '.oneicon-remove'
        err_msg = 'no remove btn found for given token in ACCESS TOKEN oz panel'
        btn = find_web_elem(self.web_elem, css_sel, err_msg)
        err_msg = 'clicking on rm btn in access token dropright disabled'
        click_on_web_elem(self._driver, btn, err_msg)
