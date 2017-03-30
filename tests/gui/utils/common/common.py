"""Utils and fixtures to facilitate operation on page objects.
"""

from tests.gui.utils.generic import click_on_web_elem
from tests.gui.utils.common.web_elements import WebObject

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class PageObject(object):
    __metaclass__ = WebObject

    def __init__(self, driver, web_elem, parent, **kwargs):
        self.web_elem = web_elem
        self.driver = driver
        self.parent = parent
        if not hasattr(self, '_click_area'):
            self._click_area = web_elem
        super(PageObject, self).__init__(**kwargs)

    def _click_on_btn(self, btn_type):
        btn = getattr(self, '_{}_btn'.format(btn_type))
        click_on_web_elem(self.driver, btn,
                          lambda: 'cannot click on {} btn in '
                                  '{}'.format(btn_type, str(self)))

    def click(self):
        click_on_web_elem(self.driver, self._click_area,
                          lambda: 'cannot click on {}'.format(str(self)))


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
        click_on_web_elem(self.driver, self._toggle,
                          lambda: 'cannot click on toggle for '
                                  '{}'.format(str(self)))
