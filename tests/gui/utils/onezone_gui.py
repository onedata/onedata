"""Utils and fixtures to facilitate operations on Onezone web GUI.
"""

import re

from pytest import fixture
from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.generic import repeat_failed, iter_ahead, find_web_elem

__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


RE_DATA_URL = re.compile(r'(?P<lang>/.*)?/data/(?P<space>.*)/(?P<dir>.*)')


@fixture()
def logout_button(selenium):
    return selenium.find_element_by_css_selector('account-menu a#nav-home.logout')


class SpaceRecord(object):

    def __init__(self, web_elem):
        self.web_elem = web_elem

    class ProviderRecord(object):
        def __init__(self, web_elem):
            self.web_elem = web_elem

        @property
        def name(self):
            css_sel = '.one-label.truncate'
            err_msg = 'unable to locate name header for supporting provider'
            header = find_web_elem(self.web_elem, css_sel, err_msg)
            return header.text

        def click(self):
            css_sel = '.clickable'
            err_msg = "can't click on '{}' supporting provider".format(self.name)
            provider = find_web_elem(self.web_elem, css_sel, err_msg)
            provider.click()

        def unsupport_space(self):
            css_sel = '.clickable .oneicon-leave-space'
            err_msg = 'no unsupport space btn found for "{}" ' \
                      'provider'.format(self.name)
            btn = find_web_elem(self.web_elem, css_sel, err_msg)
            btn.click()

    @property
    def providers(self):
        if self.is_submenu_expanded:
            css_sel = 'ul.tertiary-list li.sidebar-space-provide'
            return (SpaceRecord.ProviderRecord(provider) for provider in
                    self.web_elem.find_elements_by_css_selector(css_sel))
        else:
            raise RuntimeError('submenu for space named "{}" '
                               'is not expanded'.format(self.name))

    def __getitem__(self, provider_name):
        for provider in self.providers:
            if provider_name == provider.name:
                return provider
        else:
            raise RuntimeError('no supporting provider named "{prov}" '
                               'for space named "{space}" found'
                               ''.format(prov=provider_name, space=self.name))

    @property
    def name(self):
        css_sel = '.secondary-item-container .space-header.truncate'
        err_msg = 'cannot locate name header for given space'
        header = find_web_elem(self.web_elem, css_sel, err_msg)
        return header.text

    @property
    def size(self):
        css_sel = '.secondary-item-element.clickable .space-header-size'
        err_msg = 'no size label for space named "{}" found'.format(self.name)
        size_label = find_web_elem(self.web_elem, css_sel, err_msg)
        return size_label.text

    @property
    def providers_count(self):
        css_sel = '.secondary-item-element.providers-count .one-label'
        err_msg = 'no count supporting providers ' \
                  'label for space named "{}" found'.format(self.name)
        count_label = find_web_elem(self.web_elem, css_sel, err_msg)
        return int(count_label.text)

    @property
    def is_home(self):
        css_sel1 = '.secondary-item-element.star-toggle .oneicon-home'
        css_sel2 = '.secondary-item-element.clickable .oneicon-space-home'
        try:
            self.web_elem.find_element_by_css_selector(css_sel1)
            self.web_elem.find_element_by_css_selector(css_sel2)
        except NoSuchElementException:
            return False
        else:
            return True

    def mark_as_home(self):
        if not self.is_home:
            css = '.secondary-item-element.star-toggle .oneicon-home-outline'
            err_msg = 'no home outline found for "{}" space'.format(self.name)
            home_outline = find_web_elem(self.web_elem, css, err_msg)
            home_outline.click()

    class DropdownMenu(object):
        def __init__(self, web_elem):
            self.web_elem = web_elem

        @property
        def token(self):
            css_sel = 'input'
            err_msg = 'no input box with token found for given dropdown'
            input_box = find_web_elem(self.web_elem, css_sel, err_msg)
            return input_box.get_attribute('value')

        def copy(self):
            css_sel = 'button'
            err_msg = 'no copy button found for given dropdown'
            btn = find_web_elem(self.web_elem, css_sel, err_msg)
            btn.click()

    def get_support(self):
        if self.is_submenu_expanded:
            sp_name = self.name
            css_sel = 'ul.tertiary-list li.get-support .dropdown'
            err_msg = 'no get support btn found for "{}" space'.format(sp_name)
            btn = find_web_elem(self.web_elem, css_sel, err_msg)
            btn.click()
            if 'open' in btn.get_attribute('class'):
                css_sel2 = '{:s} .dropdown-menu'.format(css_sel)
                err_msg2 = 'no dropdown menu after clicking on get support ' \
                           'for "{}" space found'.format(sp_name)
                menu = find_web_elem(self.web_elem, css_sel2, err_msg2)
                return SpaceRecord.DropdownMenu(menu)
        else:
            raise RuntimeError('submenu for space named "{}" '
                               'is not expanded'.format(self.name))

    @property
    def is_submenu_expanded(self):
        header = self._get_space_header()
        aria_expanded = header.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    def expand_submenu(self):
        if not self.is_submenu_expanded:
            self._get_space_header().click()

    def collapse_submenu(self):
        if self.is_submenu_expanded:
            self._get_space_header().click()

    def _get_space_header(self):
        css_sel = '.secondary-item-container .clickable'
        err_msg = 'no space header found for space named "{}"'.format(self.name)
        return find_web_elem(self.web_elem, css_sel, err_msg)


class OZPanel(object):
    def __init__(self, web_elem):
        self.web_elem = web_elem

    @property
    def name(self):
        header = self._get_panel_header()
        return header.text

    @property
    def is_expanded(self):
        header = self._get_panel_header()
        return self._is_expanded(header)

    def expand(self):
        header = self._get_panel_header()
        if not self._is_expanded(header):
            header.click()

    def collapse(self):
        header = self._get_panel_header()
        if self._is_expanded(header):
            header.click()

    def _is_expanded(self, header):
        aria_expanded = header.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    def _get_panel_header(self):
        css_sel = 'a.main-accordion-toggle'
        return self.web_elem.find_element_by_css_selector(css_sel)

    def _get_btn(self, name):
        css_sel = '.clickable'
        buttons = self.web_elem.find_elements_by_css_selector(css_sel)
        for btn in buttons:
            if btn.text.lower() == name:
                return btn
        else:
            raise RuntimeError('no button named {btn} found in "{panel}" '
                               'oz panel'.format(btn=name, panel=self.name))


class DataSpaceManagementPanel(OZPanel):

    def __getitem__(self, name):
        css_sel = '.spaces-accordion-item'
        spaces = (SpaceRecord(space) for space in
                  self.web_elem.find_elements_by_css_selector(css_sel))
        for space in spaces:
            if name == space.name:
                return space
        else:
            raise RuntimeError('no space named "{space}" found in {panel} '
                               'oz panel'.format(space=name, panel=self.name))

    def join_space(self):
        btn = self._get_btn('join a space')
        btn.click()

    def create_new_space(self):
        btn = self._get_btn('create new space')
        btn.click()

    @property
    def create_space_edit_box(self):
        css_sel = '.clickable, .clickable input[id=create-new-space-name]'
        items = self.web_elem.find_elements_by_css_selector(css_sel)
        for item, next_item in iter_ahead(items):
            if next_item.tag_name == 'input':
                return EditBox(item)
        else:
            raise RuntimeError('no edit box for create new space found '
                               'in "{panel}" oz panel'.format(panel=self.name))


class EditBox(object):

    def __init__(self, web_elem):
        self.web_elem = web_elem

    @property
    def value(self):
        input_box = self._get_input_box()
        return input_box.get_attribute('value')

    @value.setter
    @repeat_failed(attempts=10)
    def value(self, text):
        input_box = self._get_input_box()
        input_box.clear()
        input_box.send_keys(text)
        assert self.value == text, 'entering {} to input box failed'.format(text)

    def confirm(self):
        css_sel = '.oneicon-checkbox-check'
        self._click_on_btn(css_sel, 'confirm')

    def cancel(self):
        css_sel = '.oneicon-checkbox-x'
        self._click_on_btn(css_sel, 'cancel')

    def _get_input_box(self):
        return find_web_elem(self.web_elem, 'input', 'no input element found '
                                                     'for given edit box')

    def _click_on_btn(self, css_sel, btn_type):
        msg = 'no {btn} button found for given edit box'.format(btn=btn_type)
        btn = find_web_elem(self.web_elem, css_sel, msg)
        btn.click()


class OZLoggedIn(object):
    panels = {'data_space_management': DataSpaceManagementPanel}

    def __init__(self, web_elem):
        self.web_elem = web_elem

    def __getattr__(self, panel):
        cls = self.panels.get(panel, None)
        if cls:
            panel = panel.replace('_', ' ').lower()
            css_sel = '.main-accordion-group, a.main-accordion-toggle'
            items = self.web_elem.find_elements_by_css_selector(css_sel)
            for group, toggle in zip(items[::2], items[1::2]):
                if panel == toggle.text.lower():
                    return cls(group)
