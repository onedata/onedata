"""Utils and fixtures to facilitate operations on
common operations between various services.
"""

from abc import ABCMeta, abstractmethod

from tests.gui.utils.core.web_elements import WebElement, WebItemsSequence, Label
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject
from tests.gui.utils.core.base import PageObject

from tests.gui.utils.generic import find_web_elem


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class OnePage(object):
    __metaclass__ = ABCMeta

    opened_tab = Label('#main-menu-container ul.main-menu '
                       'li.main-menu-item.active')
    main_menu = WebItemsSequence('#main-menu-container ul.main-menu '
                                 'li.main-menu-item',
                                 cls=ButtonWithTextPageObject)
    _sidebar = WebElement('.col-sidebar')

    @property
    def sidebar(self):
        tab = self.opened_tab
        cls = self._sidebars.get(tab, None)
        if cls is not None:
            return cls(self.driver, self._sidebar, self)
        else:
            raise RuntimeError('no {} sidebar found'.format(tab))

    def __init__(self, driver):
        self.driver = self.web_elem = driver

    @abstractmethod
    def __str__(self):
        pass


class Sidebar(PageObject):
    title = Label('.col-title')

    def __str__(self):
        return '{} sidebar in {}'.format(self.title, self.parent)


class SidebarRecord(PageObject):
    @property
    def _content(self):
        return find_web_elem(self.driver, '.col-content',
                             lambda: '{} not found'.format(self))
