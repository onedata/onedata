"""Utils and fixtures to facilitate operations on expandable elements in web GUI.
"""

from abc import ABCMeta, abstractmethod


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Expandable(object):
    __metaclass__ = ABCMeta

    @property
    def is_expanded(self):
        toggle = self._get_toggle()
        return self._is_expanded(toggle)

    def expand(self):
        toggle = self._get_toggle()
        if not self._is_expanded(toggle):
            self._click_on_toggle(toggle)

    def collapse(self):
        toggle = self._get_toggle()
        if self._is_expanded(toggle):
            self._click_on_toggle(toggle)

    # noinspection PyMethodMayBeStatic
    def _is_expanded(self, toggle):
        aria_expanded = toggle.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    @abstractmethod
    def _click_on_toggle(self, toggle):
        pass

    @abstractmethod
    def _get_toggle(self):
        pass
