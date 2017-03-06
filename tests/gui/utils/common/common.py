"""Utils and fixtures to facilitate operation on page objects.
"""

from tests.gui.utils.generic import click_on_web_elem, nth
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
        click_on_web_elem(self._driver, self._toggle,
                          lambda: 'cannot click on toggle for '
                                  '{}'.format(str(self)))


class WebItemsContainer(object):
    def __init__(self, driver, items, parent, cls):
        self.driver = driver
        self.items = items
        self.parent = parent
        self.cls = cls

    def _getitem_by_id(self, sel):
        for item in self:
            if item.id == sel:
                return item
        return None

    def _getitem_by_idx(self, idx):
        return nth(self.items, idx) if idx < len(self) else None

    def __iter__(self):
        return (self.cls(self.driver, item, self.parent)
                for item in self.items)

    def __reversed__(self):
        return (self.cls(self.driver, item, self.parent)
                for item in reversed(self.items))

    def __getitem__(self, sel):
        if isinstance(sel, int):
            item = self._getitem_by_idx(sel)
            if item:
                return self.cls(self.driver, item, self.parent)
            else:
                raise RuntimeError('Index out of bound. Requested item at '
                                   '{idx} while limit is {limit} in '
                                   '{parent}'.format(idx=sel, limit=len(self),
                                                     parent=self.parent))
        elif isinstance(sel, (str, unicode)):
            item = self._getitem_by_id(sel)
            if item:
                return item
            else:
                raise RuntimeError('no "{id}" found in '
                                   '{parent}'.format(id=sel,
                                                     parent=self.parent))

    def __contains__(self, item):
        return False if self._getitem_by_id(item) is None else True

    def __len__(self):
        return len(self.items)

    def count(self):
        return len(self)

    def index(self, requested_item):
        for i, item in enumerate(self):
            if item is requested_item:
                return i
        return None
