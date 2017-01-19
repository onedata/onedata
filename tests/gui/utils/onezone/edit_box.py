"""Utils and fixtures to facilitate operations on edit box (e.g. in USER ALIAS panel)
in Onezone web GUI.
"""

from tests.gui.utils.generic import find_web_elem, repeat_failed, click_on_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class EditBox(object):
    def __init__(self, web_elem, driver):
        self.web_elem = web_elem
        self._driver = driver

    @property
    def value(self):
        input_box = self._get_input_box()
        return input_box.get_attribute('value')

    @value.setter
    @repeat_failed(attempts=10)
    def value(self, text):
        input_box = self._get_input_box()
        input_box.clear()
        input_box.send_keys(text)
        assert self.value == text, 'entering {} to input box failed'.format(text)

    def confirm_input(self):
        css_sel = '.oneicon-checkbox-check'
        self._click_on_btn(css_sel, 'confirm')

    def cancel_input(self):
        css_sel = '.oneicon-checkbox-x'
        self._click_on_btn(css_sel, 'cancel')

    def _get_input_box(self):
        css_sel = 'input'
        err_msg = 'no input element found for given edit box'
        return find_web_elem(self.web_elem, css_sel, err_msg)

    def _click_on_btn(self, css_sel, btn_type):
        msg = 'no {btn} button found for given edit box'.format(btn=btn_type)
        btn = find_web_elem(self.web_elem, css_sel, msg)
        err_msg = 'clicking on {} btn in edit box disabled'.format(btn_type)
        click_on_web_elem(self._driver, btn, err_msg)
