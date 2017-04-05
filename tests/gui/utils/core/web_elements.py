"""Utils and fixtures to facilitate operations on various web elements in web GUI.
"""

from abc import abstractmethod, ABCMeta
from functools import partial

from .web_objects import ButtonWebObject, WebObjectsSequence, InputWebObject, \
    ButtonWithTextWebObject
from tests.gui.utils.generic import find_web_elem, find_web_elem_with_text

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class WebObject(type):
    def __init__(cls, cls_name, bases, cls_dict):
        for key, val in cls_dict.items():
            if isinstance(val, AbstractWebElement) and \
                    (val.name in ('id', '')):
                val.name = key
        super(WebObject, cls).__init__(cls_name, bases, cls_dict)


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
        self.cls = keyword_only_arg(kwargs, 'cls')
        super(AbstractWebItem, self).__init__(*args, **kwargs)


class WebElement(AbstractWebElement):
    def __init__(self, *args, **kwargs):
        self.parent_name = kwargs.pop('parent_name', '')
        super(WebElement, self).__init__(*args, **kwargs)

    def __get__(self, instance, owner):
        if instance is None:
            return self

        return find_web_elem(instance.web_elem, self.css_sel,
                             lambda: self._format_msg('no {item} item found '
                                                      'in {parent}', instance))

    def _format_msg(self, err_msg, parent, **kwargs):
        name = self.name.replace('_', ' ').strip().upper()
        p_name = (self.parent_name if self.parent_name != '' else str(parent))
        return err_msg.format(item=name, parent=p_name, **kwargs)


class WebElementWithText(WebElement):
    def __init__(self, *args, **kwargs):
        self.text = keyword_only_arg(kwargs, 'text')
        super(WebElementWithText, self).__init__(*args, **kwargs)

    def __get__(self, instance, owner):
        if instance is None:
            return self

        err_msg = 'no {item} with "{text}" text found in {parent}'
        return find_web_elem_with_text(instance.web_elem,
                                       self.css_sel, self.text,
                                       lambda: self._format_msg(err_msg, instance,
                                                                text=self.text))


class WebItem(AbstractWebItem, WebElement):
    def __get__(self, instance, owner):
        elem = super(WebItem, self).__get__(instance, owner)
        return elem if instance is None else self.cls(instance.driver,
                                                      elem, parent=instance,
                                                      name=self.name)


Input = partial(WebItem, cls=InputWebObject)
Button = partial(WebItem, cls=ButtonWebObject)


class WebItemWithText(WebItem, WebElementWithText):
    pass


NamedButton = partial(WebItemWithText, cls=ButtonWithTextWebObject)


def keyword_only_arg(kwargs, arg):
    try:
        return kwargs.pop(arg)
    except KeyError:
        raise ValueError('{} argument not specified'.format(arg))


class ButtonWithTextWebElement(WebElement):
    item_not_found_msg = '{text} btn not found in {parent}'

    def __init__(self, *args, **kwargs):
        if 'text' in kwargs:
            self._text = kwargs.pop('text')
        else:
            raise ValueError('text argument not specified')

        super(ButtonWithTextWebElement, self).__init__(*args, **kwargs)

    def __get__(self, instance, owner):
        if instance is None:
            return self

        msg = self.item_not_found_msg
        return find_web_elem_with_text(instance.web_elem, self.css_sel,
                                       self._text,
                                       lambda: self._format_msg(msg, instance))


class WebElementsSequence(AbstractWebElement):
    def __get__(self, instance, owner):
        if instance is None:
            return self

        return instance.web_elem.find_elements_by_css_selector(self.css_sel)


class WebItemsSequence(AbstractWebItem, WebElementsSequence):
    def __get__(self, instance, owner):
        items = super(WebItemsSequence, self).__get__(instance, owner)
        if instance is None:
            return items
        else:
            return WebObjectsSequence(instance.driver, items,
                                      instance, self.cls)


class ItemListWebElement(WebElement):
    def __get__(self, instance, owner):
        if instance is None:
            return self

        return instance.web_elem.find_elements_by_css_selector(self.css_sel)


class WebElementsSequenceItemWithText(object):
    def __init__(self, seq, text, cls):
        self.seq = seq
        self.text = text.lower()
        self.cls = cls

    def __get__(self, instance, owner):
        for item in self.seq.__get__(instance, owner):
            if item.text.lower() == self.text:
                return self.cls(instance.driver, item, instance)


class TextLabelWebElement(WebElement):
    item_not_found_msg = '{item} label not found in {parent}'

    def __get__(self, instance, owner):
        item = super(TextLabelWebElement, self).__get__(instance, owner)
        return item.text if instance else item
