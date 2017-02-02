"""Utils and fixtures to facilitate operations on space selector in
data tab in oneprovider web GUI.
"""

from tests.gui.utils.common.expandable import Expandable
from tests.gui.utils.generic import click_on_web_elem, find_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class SpaceSelector(Expandable):
    def __init__(self, driver, web_elem):
        self._driver = driver
        self.web_elem = web_elem

    @property
    def selected_space_name(self):
        err_msg = 'unable to locate name label for selected space in ' \
                  'space selector in data tab in op'
        label = find_web_elem(self.web_elem, '.item-label', err_msg)
        return label.text

    @property
    def is_selected_space_home(self):
        err_msg = 'unable to locate icon for selected space named "{}" in space ' \
                  'selector in data tab in op'.format(self.selected_space_name)
        icon = find_web_elem(self.web_elem, '.item-icon .one-icon', err_msg)
        return 'oneicon-space-home' in icon.get_attribute('class')

    def _click_on_toggle(self, toggle):
        err_msg = 'clicking on toggle for space selector in data tab in op' \
                  'disabled'
        click_on_web_elem(self._driver, toggle, err_msg)

    def _get_toggle(self):
        css_sel = 'a.dropdown-toggle'
        err_msg = 'unable to locate toggle for space selector in data tab in op'
        return find_web_elem(self.web_elem, css_sel, err_msg)

    @property
    def spaces(self):
        if self.is_expanded:
            css_sel = 'ul.dropdown-menu-list li'
            return [SpaceRecord(self._driver, space) for space
                    in self.web_elem.find_elements_by_css_selector(css_sel)]
        else:
            raise RuntimeError('dropdown menu for space selector in data '
                               'tab in op is not expanded')

    def __getitem__(self, name):
        for space in self.spaces:
            if name == space.name:
                return space
        else:
            raise RuntimeError('no space named "{space}" displayed in expanded '
                               'dropdown menu for space selector in data tab '
                               'in op found'.format(space=name))


class SpaceRecord(object):
    def __init__(self, driver, web_elem):
        self._driver = driver
        self.web_elem = web_elem

    @property
    def name(self):
        err_msg = 'unable to locate name label for given space in ' \
                  'expanded list for space selector in data tab in op'
        label = find_web_elem(self.web_elem, '.item-label', err_msg)
        return label.text

    @property
    def is_home(self):
        css_sel = '.item-icon .one-icon'
        err_msg = 'unable to locate icon for space named "{}" in ' \
                  'expanded list for space selector ' \
                  'in data tab in op'.format(self.name)
        icon = find_web_elem(self.web_elem, css_sel, err_msg)
        return 'oneicon-space-home' in icon.get_attribute('class')

    def click(self):
        err_msg = 'clicking on space record named "{}" in space selector ' \
                  'in data tab in op disabled'.format(self.name)
        click_on_web_elem(self._driver, self.web_elem, err_msg)
