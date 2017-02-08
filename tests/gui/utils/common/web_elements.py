"""Utils and fixtures to facilitate operations on various web elements in web GUI.
"""

from tests.gui.utils.generic import find_web_elem, repeat, find_web_elem_with_text

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


def web_item(cls):
    return cls


class WebItem(type):
    def __init__(cls, cls_name, bases, cls_dict):
        for key, val in cls_dict.items():
            if isinstance(val, WebElement) and val.name is None:
                val.name = key
        super(WebItem, cls).__init__(cls_name, bases, cls_dict)


class WebElement(object):
    item_not_found_msg = '{item} item not found in {parent}'

    def __init__(self, css_sel, name=None, parent_name=None, **kwargs):
        self.name = name
        self._parent_name = parent_name
        self.css_sel = css_sel
        super(WebElement, self).__init__(**kwargs)

    def __get__(self, instance, owner):
        if instance is None:
            return self

        parent_name = self._get_parent_name(instance)
        err_msg = self.item_not_found_msg.format(item=self._get_name(),
                                                 parent=parent_name)
        return find_web_elem(instance.web_elem, self.css_sel, err_msg)

    def _get_name(self):
        return self.name.replace('_', ' ').strip().upper()

    def _get_parent_name(self, parent):
        return str(self._parent_name
                   if self._parent_name is not None
                   else parent)


class IconWebElement(WebElement):
    item_not_found_msg = '{item} icon not found in {parent}'


class ToggleWebElement(WebElement):
    item_not_found_msg = '{item} toggle not found in {parent}'


class ButtonWebElement(WebElement):
    item_not_found_msg = '{item} btn not found in {parent}'


class HeaderWebElement(WebElement):
    item_not_found_msg = '{item} header not found in {parent}'


class InputWebElement(WebElement):
    item_not_found_msg = '{item} input box not found in {parent}'
    typing_text_failed_msg = 'entering {value} to {item} input box in ' \
                             '{parent} failed'

    def __get__(self, instance, owner):
        item = self._get_input_box(instance, owner)
        return item.get_attribute('value') if instance else item

    @repeat(attempts=10)
    def __set__(self, instance, value):
        parent_name = self._get_parent_name(instance)
        err_msg = self.typing_text_failed_msg.format(value=value,
                                                     item=self.name,
                                                     parent=parent_name)
        input_box = self._get_input_box(instance, None)
        input_box.clear()
        input_box.send_keys(value)
        assert getattr(instance, self.name) == value, err_msg

    def _get_input_box(self, instance, owner):
        return super(InputWebElement, self).__get__(instance, owner)


class TextLabelWebElement(WebElement):
    item_not_found_msg = '{item} label not found in {parent}'

    def __get__(self, instance, owner):
        item = super(TextLabelWebElement, self).__get__(instance, owner)
        return item.text if instance else item


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

        parent_name = self._get_parent_name(instance)
        err_msg = self.item_not_found_msg.format(text=self._text,
                                                 parent=parent_name)
        return find_web_elem_with_text(instance.web_elem, self.css_sel,
                                       self._text, err_msg)


class ItemListWebElement(WebElement):
    def __get__(self, instance, owner):
        if instance is None:
            return self

        return instance.web_elem.find_elements_by_css_selector(self.css_sel)
