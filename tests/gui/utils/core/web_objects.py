"""Utils and fixtures to facilitate operations on various web objects in web GUI.
"""

from abc import abstractmethod, ABCMeta

from tests.gui.utils.generic import nth, click_on_web_elem, repeat_failed

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class AbstractWebObject(object):
    __metaclass__ = ABCMeta

    def __init__(self, driver, web_elem, parent=None, name=''):
        self.driver = driver
        self.web_elem = web_elem
        self.parent = parent
        if name != '':
            self.name = name

    @abstractmethod
    def __str__(self):
        pass


class InputWebObject(AbstractWebObject):
    name = 'input'

    def __str__(self):
        return '{} in {}'.format(self.name, self.parent)

    @property
    def value(self):
        return self.web_elem.get_attribute('value')

    @value.setter
    @repeat_failed(attempts=10)
    def value(self, val):
        self.clear()
        self.web_elem.send_keys(val)
        assert self.value == val, 'entering "{}" to {} failed'.format(val, self)

    def clear(self):
        self.web_elem.clear()


class ButtonWebObject(AbstractWebObject):
    name = 'button'

    def __str__(self):
        return '{} btn in {}'.format(self.name, self.parent)

    def __call__(self, *args, **kwargs):
        click_on_web_elem(self.driver, self.web_elem,
                          lambda: 'cannot click on {btn}'.format(btn=self))

    def is_enabled(self):
        return self.web_elem.is_enabled()

    def is_active(self):
        return 'active' in self.web_elem.get_attribute('class')


class ButtonWithTextWebObject(ButtonWebObject):
    def __str__(self):
        return '{} btn with "{}" text in {}'.format(self.name, self.text,
                                                    self.parent)

    @property
    def text(self):
        return self.web_elem.text

    id = text


class WebObjectsSequence(object):

    def __init__(self, driver, items, cls, parent=None):
        self.driver = driver
        self.items = items
        self.cls = cls
        self.parent = parent

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
        else:
            return None

    def __contains__(self, item):
        if isinstance(item, self.cls):
            item = item.id
        return False if self._getitem_by_id(item) is None else True

    def __len__(self):
        return len(self.items)

    def count(self):
        return len(self)

    def index(self, item_for_idx):
        if isinstance(item_for_idx, self.cls):
            item_searched = item_for_idx.id
        else:
            item_searched = item_for_idx

        for i, item in enumerate(self):
            if item.id == item_searched:
                return i
        return None
