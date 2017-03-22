"""Utils and fixtures to facilitate operations on various web elements in web GUI.
"""

from abc import abstractmethod, ABCMeta

from tests.gui.utils.core.web_objects import ButtonWebObject, WebObjectsSequence
from tests.gui.utils.generic import find_web_elem, repeat_failed, find_web_elem_with_text

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class WebObject(type):
    def __init__(cls, cls_name, bases, cls_dict):
        for key, val in cls_dict.items():
            if isinstance(val, AbstractWebElement) and \
                    (val.name is None or val.name == 'id'):
                val.name = key
        super(WebObject, cls).__init__(cls_name, bases, cls_dict)


class AbstractWebElement(object):
    __metaclass__ = ABCMeta

    def __init__(self, css_sel, name=None):
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
        if 'cls' in kwargs:
            self.cls = kwargs.pop('cls')
        else:
            raise ValueError('cls argument not specified')

        super(AbstractWebItem, self).__init__(*args, **kwargs)


class WebElement(AbstractWebElement):
    item_not_found_msg = '{item} element not found in {parent}'

    def __init__(self, *args, **kwargs):
        self.parent_name = kwargs.pop('parent_name', None)
        super(WebElement, self).__init__(*args, **kwargs)

    def __get__(self, instance, owner):
        if instance is None:
            return self

        return find_web_elem(instance.web_elem, self.css_sel,
                             lambda: self._format_msg(self.item_not_found_msg,
                                                      instance))

    def _format_msg(self, msg_template, parent):
        name = self.name.replace('_', ' ').strip().upper()
        parent_name = (self.parent_name
                       if self.parent_name is not None
                       else str(parent))
        return msg_template.format(item=name, parent=parent_name)


class WebElementsSequence(AbstractWebElement):
    def __get__(self, instance, owner):
        if instance is None:
            return self

        return instance.web_elem.find_elements_by_css_selector(self.css_sel)


class WebItem(AbstractWebItem, WebElement):
    item_not_found_msg = '{item} item not found in {parent}'

    def __get__(self, instance, owner):
        elem = super(WebItem, self).__get__(instance, owner)
        if instance is None:
            return elem
        else:
            return self.cls(instance.driver, elem, instance)


class WebItemsSequence(AbstractWebItem, WebElementsSequence):
    def __get__(self, instance, owner):
        items = super(WebItemsSequence, self).__get__(instance, owner)
        if instance is None:
            return items
        else:
            return WebObjectsSequence(instance.driver, items,
                                      instance, self.cls)


class InputWebElement(WebElement):
    item_not_found_msg = '{item} input box not found in {parent}'
    typing_text_failed_msg = 'entering {value} to {item} input box in ' \
                             '{parent} failed'

    def __get__(self, instance, owner):
        item = super(InputWebElement, self).__get__(instance, owner)
        return item.get_attribute('value') if instance else item

    @repeat_failed(attempts=10)
    def __set__(self, instance, value):
        input_box = super(InputWebElement, self).__get__(instance,
                                                         type(instance))
        input_box.clear()
        input_box.send_keys(value)
        assert self.__get__(instance, type(instance)) == value, \
            lambda: self._format_msg(self.typing_text_failed_msg, instance)


class TextLabelWebElement(WebElement):
    item_not_found_msg = '{item} label not found in {parent}'

    def __get__(self, instance, owner):
        item = super(TextLabelWebElement, self).__get__(instance, owner)
        return item.text if instance else item


class ButtonWebItem(WebItem):
    item_not_found_msg = '{text} btn not found in {parent}'

    def __init__(self, *args, **kwargs):
        super(ButtonWebItem, self).__init__(cls=ButtonWebObject, *args, **kwargs)

    def __get__(self, instance, owner):
        btn = super(ButtonWebItem, self).__get__(instance, owner)
        btn.name = self.name
        return btn


class IconWebElement(WebElement):
    item_not_found_msg = '{item} icon not found in {parent}'


class ToggleWebElement(WebElement):
    item_not_found_msg = '{item} toggle not found in {parent}'


class ButtonWebElement(WebElement):
    item_not_found_msg = '{item} btn not found in {parent}'


class HeaderWebElement(WebElement):
    item_not_found_msg = '{item} header not found in {parent}'


class ModalWebElement(WebElement):
    item_not_found_msg = '{item} modal not found in {parent}'


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


class ItemListWebElement(WebElement):
    def __get__(self, instance, owner):
        if instance is None:
            return self

        return instance.web_elem.find_elements_by_css_selector(self.css_sel)
