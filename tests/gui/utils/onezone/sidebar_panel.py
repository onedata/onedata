"""Utils and fixtures to facilitate operations on panels (base for specific panels)
in Onezone web GUI.
"""

from tests.gui.utils.generic import find_web_elem_with_text, click_on_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class OZPanel(object):
    def __init__(self, web_elem, driver):
        self.web_elem = web_elem
        self._driver = driver

    @property
    def name(self):
        header = self._get_panel_header()
        return header.text

    @property
    def is_expanded(self):
        header = self._get_panel_header()
        return self._is_expanded(header)

    def expand(self):
        header = self._get_panel_header()
        if not self._is_expanded(header):
            self._click_on_toggle(header)

    def collapse(self):
        header = self._get_panel_header()
        if self._is_expanded(header):
            self._click_on_toggle(header)

    def _click_on_toggle(self, toggle):
        err_msg = 'clicking on toggle for "{}" panel ' \
                  'is disabled'.format(self.name)
        click_on_web_elem(self._driver, toggle, err_msg)

    # noinspection PyMethodMayBeStatic
    def _is_expanded(self, header):
        aria_expanded = header.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    def _get_panel_header(self):
        css_sel = 'a.main-accordion-toggle'
        return self.web_elem.find_element_by_css_selector(css_sel)

    def _get_btn(self, name):
        css_sel = '.clickable'
        err_msg = 'no button named {btn} found in "{panel}" ' \
                  'oz panel'.format(btn=name, panel=self.name)
        return find_web_elem_with_text(self.web_elem, css_sel,
                                       name, err_msg)
