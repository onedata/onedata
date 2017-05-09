"""Utils and fixtures to facilitate operations on various web objects in web GUI.
"""

from abc import abstractmethod, ABCMeta

from tests.gui.utils.generic import click_on_web_elem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class AbstractWebElement(object):
    __metaclass__ = ABCMeta

    def __init__(self, css_sel, name=''):
        self.css_sel = css_sel
        self.name = name

    def __delete__(self, instance):
        raise AttributeError("can't delete attribute")

    def __set__(self, instance, value):
        raise AttributeError("can't set attribute")

    @abstractmethod
    def __get__(self, instance, owner):
        pass


class AbstractWebItem(AbstractWebElement):
    __metaclass__ = ABCMeta

    def __init__(self, *args, **kwargs):
        self.cls = kwargs.pop('cls', None)
        if self.cls is None:
            raise ValueError('cls not specified')
        super(AbstractWebItem, self).__init__(*args, **kwargs)


class PageObjectMeta(ABCMeta):
    def __init__(cls, cls_name, bases, cls_dict):
        for key, val in cls_dict.items():
            if isinstance(val, AbstractWebElement) and val.name in ('id', ''):
                val.name = key
        super(PageObjectMeta, cls).__init__(cls_name, bases, cls_dict)


class AbstractPageObject(object):
    __metaclass__ = PageObjectMeta

    def __init__(self, driver, web_elem, parent=None, name=''):
        self.driver = driver
        self.web_elem = web_elem
        self.parent = parent
        if name != '':
            self.name = name

    @abstractmethod
    def __str__(self):
        pass


class PageObject(AbstractPageObject):
    def __init__(self, driver, web_elem, parent=None, **kwargs):
        super(PageObject, self).__init__(driver, web_elem, parent, **kwargs)
        if not hasattr(self, '_click_area'):
            self._click_area = web_elem

    def __str__(self):
        return 'web object'

    def click(self):
        click_on_web_elem(self.driver, self._click_area,
                          lambda: 'cannot click on {}'.format(self))


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
                          lambda: 'cannot click on toggle for {}'.format(self))
