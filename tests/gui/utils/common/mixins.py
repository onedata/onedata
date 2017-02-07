"""Utils and fixtures to facilitate operations on expandable elements in web GUI.
"""

from selenium.webdriver import ActionChains

from tests.gui.utils.generic import click_on_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class ExpandableMixin(object):
    __slots__ = ()

    def is_expanded(self):
        aria_expanded = self._toggle.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    def expand(self):
        if not self.is_expanded():
            self._click_on_toggle()

    def collapse(self):
        if self.is_expanded():
            self._click_on_toggle()

    def _click_on_toggle(self):
        err_msg = 'clicking on toggle for {} disabled'.format(str(self))
        click_on_web_elem(self._driver, self._toggle, err_msg)


class ClickableMixin(object):
    __slots__ = ()

    def click(self):
        err_msg = 'clicking on {} disabled'.format(str(self))
        click_on_web_elem(self._driver, self.web_elem, err_msg)

    def double_click(self):
        ActionChains(self._driver).double_click(self.web_elem).perform()
