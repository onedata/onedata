"""Utils and fixtures to facilitate operations on various web elements in web GUI.
"""

from tests.gui.utils.generic import find_web_elem, repeat, find_web_elem_with_text

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


def web_item(cls):
    for key, val in vars(cls).items():
        if isinstance(val, WebElement) and val.name is None:
            val.name = key
    return cls


class WebElement(object):
    item_not_found_msg = '{item} item not found in {parent}'

    def __init__(self, css_sel, name=None, **kwargs):
        self.name = name
        self.css_sel = css_sel
        super(WebElement, self).__init__(**kwargs)

    def __get__(self, instance, owner):
        if instance is None:
            return self

        err_msg = self.item_not_found_msg.format(item=self.name,
                                                 parent=str(instance))
        return find_web_elem(instance.web_elem, self.css_sel, err_msg)


class InputWebElement(WebElement):
    item_not_found_msg = '{item} input box not found in {parent}'
    typing_text_failed_msg = 'entering {value} to {item} input box in ' \
                             '{parent} failed'

    def __get__(self, instance, owner):
        if instance is None:
            return self

        input_box = self._get_input_box(instance)
        return input_box.get_attribute('value')

    @repeat(attempts=10)
    def __set__(self, instance, value):
        err_msg = self.typing_text_failed_msg.format(value=value,
                                                     item=self.name,
                                                     parent=str(instance))
        input_box = self._get_input_box(instance)
        input_box.clear()
        input_box.send_keys(value)
        assert getattr(instance, self.name) == value, err_msg

    def _get_input_box(self, obj):
        err_msg = self.item_not_found_msg.format(item=self.name,
                                                 parent=str(obj))
        return find_web_elem(obj.web_elem, self.css_sel, err_msg)


class TextLabelWebElement(WebElement):
    item_not_found_msg = '{item} label not found in {parent}'

    def __init__(self, *args, **kwargs):
        self._parent_name = None
        if 'parent_name' in kwargs:
            self._parent_name = kwargs.pop('parent_name')

        super(TextLabelWebElement, self).__init__(*args, **kwargs)

    def __get__(self, instance, owner):
        if instance is None:
            return self

        parent_name = self._parent_name if self._parent_name else str(instance)
        err_msg = self.item_not_found_msg.format(item=self.name,
                                                 parent=parent_name)
        return find_web_elem(instance.web_elem, self.css_sel, err_msg).text


class IconWebElement(WebElement):
    item_not_found_msg = '{item} icon not found in {parent}'


class ToggleWebElement(WebElement):
    item_not_found_msg = '{item} toggle not found in {parent}'


class ButtonWebElement(WebElement):
    item_not_found_msg = '{item} btn not found in {parent}'


class ButtonWithTextWebElement(WebElement):
    item_not_found_msg = '{text} btn not found in {parent}'

    def __init__(self, *args, **kwargs):
        if 'text' in kwargs:
            self._text = kwargs.pop('text')
        if self._text is None:
            raise ValueError('')

        super(ButtonWithTextWebElement, self).__init__(*args, **kwargs)

    def __get__(self, instance, owner):
        if instance is None:
            return self

        err_msg = self.item_not_found_msg.format(text=self._text,
                                                 parent=str(instance))
        return find_web_elem_with_text(instance.web_elem, self.css_sel,
                                       self._text, err_msg)


class ItemListWebElement(WebElement):
    def __get__(self, instance, owner):
        if instance is None:
            return self

        return instance.web_elem.find_elements_by_css_selector(self.css_sel)
