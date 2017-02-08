"""Utils and fixtures to facilitate operation on page objects.
"""

from tests.gui.utils.common.web_elements import WebItem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class PageObject(object):
    __metaclass__ = WebItem

    def __init__(self, driver, web_elem, parent, **kwargs):
        self.web_elem = web_elem
        self._driver = driver
        self._parent = parent
        super(PageObject, self).__init__(**kwargs)
