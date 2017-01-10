"""Utils and fixtures to facilitate basic operations on records in Onezone panel web GUI.
"""

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.generic import find_web_elem


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class OZPanelRecord(object):
    _name_css = '.secondary-item-container .{}-header.truncate'
    _home_icon_css = '.oneicon-{}-home'
    _submenu_toggle_css = '.secondary-item-container {}'

    def __init__(self, web_elem, record_type, submenu_toggle):
        self.web_elem = web_elem
        self._type = record_type
        self._name_css = self._name_css.format(record_type)
        self._home_icon_css = self._home_icon_css.format(record_type)
        self._submenu_toggle_css = self._submenu_toggle_css.format(submenu_toggle)

    def __eq__(self, other):
        if isinstance(other, str) or isinstance(other, unicode):
            return self.name == other
        else:
            raise NotImplementedError('operation not implemented')

    @property
    def name(self):
        err_msg = 'cannot locate name header for given {}'.format(self._type)
        header = find_web_elem(self.web_elem, self._name_css, err_msg)
        return header.text

    @property
    def is_home(self):
        css_sel = '.secondary-item-element.star-toggle .oneicon-home'
        try:
            self.web_elem.find_element_by_css_selector(css_sel)
            self.web_elem.find_element_by_css_selector(self._home_icon_css)
        except NoSuchElementException:
            return False
        else:
            return True

    def set_as_home(self):
        if not self.is_home:
            css = '.secondary-item-element.star-toggle .oneicon-home-outline'
            err_msg = 'no home outline found for {type} named' \
                      '"{name}"'.format(name=self.name, type=self._type)
            home_outline = find_web_elem(self.web_elem, css, err_msg)
            home_outline.click()

    @property
    def is_submenu_expanded(self):
        toggle = self._get_submenu_toggle()
        return self._is_submenu_expanded(toggle)

    def expand_submenu(self):
        toggle = self._get_submenu_toggle()
        if not self._is_submenu_expanded(toggle):
            toggle.click()

    def collapse_submenu(self):
        toggle = self._get_submenu_toggle()
        if self._is_submenu_expanded(toggle):
            toggle.click()

    def _is_submenu_expanded(self, toggle):
        aria_expanded = toggle.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    def _get_submenu_toggle(self):
        err_msg = 'no record header found for {type} ' \
                  'named "{name}"'.format(type=self._type, name=self.name)
        return find_web_elem(self.web_elem, self._submenu_toggle_css, err_msg)
