"""Utils and fixtures to facilitate operations on Onezone web GUI.
"""

import re

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.generic import repeat_failed, iter_ahead, find_web_elem


__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


RE_DATA_URL = re.compile(r'(?P<lang>/.*)?/data/(?P<space>.*)/(?P<dir>.*)')


class OZPanelRecord(object):
    _name_css = '.secondary-item-container .{}-header.truncate'
    _home_icon_css = '.oneicon-{}-home'
    _submenu_toggle_css = '.secondary-item-container {}'

    def __init__(self, web_elem, record_type, submenu_toggle='.clickable'):
        self.web_elem = web_elem
        self._name_css = self._name_css.format(record_type)
        self._home_icon_css = self._home_icon_css.format(record_type)
        self._submenu_toggle_css = self._submenu_toggle_css.format(submenu_toggle)
        self._type = record_type

    @property
    def name(self):
        err_msg = 'cannot locate name header for given {}'.format(self._type)
        header = find_web_elem(self.web_elem, self._name_css, err_msg)
        return header.text

    @property
    def is_home(self):
        css_sel1 = '.secondary-item-element.star-toggle .oneicon-home'
        try:
            self.web_elem.find_element_by_css_selector(css_sel1)
            self.web_elem.find_element_by_css_selector(self._home_icon_css)
        except NoSuchElementException:
            return False
        else:
            return True

    def set_as_home(self):
        if not self.is_home:
            css = '.secondary-item-element.star-toggle .oneicon-home-outline'
            err_msg = 'no home outline found for ' \
                      '"{name}" {type}'.format(name=self.name,
                                               type=self._type)
            home_outline = find_web_elem(self.web_elem, css, err_msg)
            home_outline.click()

    @property
    def is_submenu_expanded(self):
        toggle = self._get_submenu_toggle()
        return self._is_submenu_expanded(toggle)

    def expand_submenu(self):
        toggle = self._get_submenu_toggle()
        if not self._is_submenu_expanded(toggle):
            toggle.click()

    def collapse_submenu(self):
        toggle = self._get_submenu_toggle()
        if self._is_submenu_expanded(toggle):
            toggle.click()

    def _is_submenu_expanded(self, toggle):
        aria_expanded = toggle.get_attribute('aria-expanded')
        return True if (aria_expanded and 'true' == aria_expanded) else False

    def _get_submenu_toggle(self):
        err_msg = 'no space header found for {type} ' \
                  'named "{name}"'.format(type=self._type, name=self.name)
        return find_web_elem(self.web_elem, self._submenu_toggle_css, err_msg)


class SpaceRecord(OZPanelRecord):

    class ProviderRecord(object):
        def __init__(self, web_elem):
            self.web_elem = web_elem

        @property
        def name(self):
            css_sel = '.one-label.truncate'
            err_msg = 'unable to locate name header for supporting provider ' \
                      'for given space in DATA SPACE MANAGEMENT oz panel'
            header = find_web_elem(self.web_elem, css_sel, err_msg)
            return header.text

        def click(self):
            css_sel = '.clickable'
            err_msg = "can't click on '{}' supporting provider for given space " \
                      "in DATA SPACE MANAGEMENT oz panel".format(self.name)
            provider = find_web_elem(self.web_elem, css_sel, err_msg)
            provider.click()

        def unsupport_space(self):
            css_sel = '.clickable .oneicon-leave-space'
            err_msg = 'no unsupport space btn found for "{}" ' \
                      'provider for given space in ' \
                      'DATA SPACE MANAGEMENT oz panel'.format(self.name)
            btn = find_web_elem(self.web_elem, css_sel, err_msg)
            btn.click()

    @property
    def supporting_providers(self):
        if self.is_submenu_expanded:
            css_sel = 'ul.tertiary-list li.sidebar-space-provide'
            return (SpaceRecord.ProviderRecord(provider) for provider in
                    self.web_elem.find_elements_by_css_selector(css_sel))
        else:
            raise RuntimeError('submenu for space named "{}" '
                               'is not expanded in DATA SPACE MANAGEMENT '
                               'oz panel'.format(self.name))

    def __getitem__(self, provider_name):
        for provider in self.supporting_providers:
            if provider_name == provider.name:
                return provider
        else:
            raise RuntimeError('no supporting provider named "{prov}" '
                               'for space named "{space}" found in DATA SPACE '
                               'MANAGEMENT oz panel'.format(prov=provider_name,
                                                            space=self.name))

    @property
    def size(self):
        css_sel = '.secondary-item-element.clickable .space-header-size'
        err_msg = 'no size label for space named "{}" found in ' \
                  'DATA SPACE MANAGEMENT oz panel'.format(self.name)
        size_label = find_web_elem(self.web_elem, css_sel, err_msg)
        return size_label.text

    @property
    def providers_count(self):
        css_sel = '.secondary-item-element.providers-count .one-label'
        err_msg = 'no count supporting providers ' \
                  'label for space named "{}" found in DATA SPACE MANAGEMENT ' \
                  'oz panel'.format(self.name)
        count_label = find_web_elem(self.web_elem, css_sel, err_msg)
        return int(count_label.text)

    class SpaceSupportTokenDropdownMenu(object):
        def __init__(self, web_elem):
            self.web_elem = web_elem

        @property
        def token(self):
            css_sel = 'input'
            err_msg = 'no input box with token found for ' \
                      'given space support token dropdown'
            input_box = find_web_elem(self.web_elem, css_sel, err_msg)
            return input_box.get_attribute('value')

        def copy(self):
            css_sel = 'button'
            err_msg = 'no copy button found for ' \
                      'given space support token dropdown'
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
                           'for "{}" space found in ' \
                           'DATA SPACE MANAGEMENT'.format(sp_name)
                menu = find_web_elem(self.web_elem, css_sel2, err_msg2)
                return SpaceRecord.SpaceSupportTokenDropdownMenu(menu)
        else:
            raise RuntimeError('submenu for space named "{}" in DATA SPACE '
                               'MANAGEMENT is not expanded'.format(self.name))


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

    @property
    def spaces(self):
        css_sel = '.spaces-accordion-item'
        return (SpaceRecord(space, 'space') for space in
                self.web_elem.find_elements_by_css_selector(css_sel))

    def __getitem__(self, name):
        for space in self.spaces:
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


class SpaceRecordInProvidersPanel(object):
    def __init__(self, web_elem, name_css, size_css):
        self.web_elem = web_elem
        self.name_css = name_css
        self.size_css = size_css

    @property
    def name(self):
        err_msg = 'unable to locate name header for supported space in ' \
                  'displayed provider panel'
        header = find_web_elem(self.web_elem, self.name_css, err_msg)
        return header.text

    @property
    def size(self):
        err_msg = 'no size label for space named "{}" found ' \
                  'in displayed provider panel'.format(self.name)
        size_label = find_web_elem(self.web_elem, self.size_css, err_msg)
        return size_label.text

    @property
    def is_home(self):
        css_sel = '.oneicon-space-default'
        try:
            self.web_elem.find_element_by_css_selector(css_sel)
        except NoSuchElementException:
            return False
        else:
            return True


class ProviderRecord(OZPanelRecord):

    @property
    def is_working(self):
        css_sel = '.provider-icon .color-provider-working'
        try:
            self.web_elem.find_element_by_css_selector(css_sel)
        except NoSuchElementException:
            return False
        else:
            return True

    @property
    def spaces_count(self):
        css_sel = '.spaces-count'
        err_msg = 'no spaces count for "{}" provider found in ' \
                  'GO TO YOUR FILES panel'.format(self.name)
        spaces_count = find_web_elem(self.web_elem, css_sel, err_msg)
        return int(spaces_count)

    def click_on(self):
        css_sel = '.secondary-item-container'
        err_msg = 'no provider header for "{}" found in GO TO YOUR FILES ' \
                  'panel'.format(self.name)
        header = find_web_elem(self.web_elem, css_sel, err_msg)
        header.click()

    def unset_from_home(self):
        css_sel = '.secondary-item-element.star-toggle .oneicon-home'
        err_msg = 'no home icon found for "{}" provider in GO TO YOUR FILES ' \
                  'panel'.format(self.name)
        home_icon = find_web_elem(self.web_elem, css_sel, err_msg)
        home_icon.click()

    @property
    def supported_spaces(self):
        if self.is_submenu_expanded:
            css_sel = 'ul.tertiary-list li.sidebar-provider-space'
            return (SpaceRecordInProvidersPanel(space,
                                                name_css='.one-label.truncate',
                                                size_css='.space-header-size')
                    for space
                    in self.web_elem.find_elements_by_css_selector(css_sel))
        else:
            raise RuntimeError('submenu for provider named "{}" '
                               'is not expanded in GO TO YOUR FILES '
                               'panel'.format(self.name))

    def __getitem__(self, space_name):
        for space in self.supported_spaces:
            if space_name == space.name:
                return space
        else:
            raise RuntimeError('no supported space named "{space}" '
                               'for provider named "{provider}" in '
                               'GO TO YOUR FILES panel '
                               'found'.format(provider=self.name,
                                              space=space_name))


class GoToYourFilesPanel(OZPanel):

    @property
    def providers(self):
        css_sel = '#providers-list .providers-accordion-item'
        return (ProviderRecord(provider, 'provider', '.spaces-count')
                for provider
                in self.web_elem.find_elements_by_css_selector(css_sel))

    def __getitem__(self, name):
        for provider in self.providers:
            if name == provider.name:
                return provider
        else:
            raise RuntimeError('no provider named "{prov}" found in {panel} '
                               'oz panel'.format(prov=name, panel=self.name))


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
    panels = {'data_space_management': DataSpaceManagementPanel,
              'go_to_your_files': GoToYourFilesPanel}

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


class ProviderDropPanel(object):

    def __init__(self, web_elem):
        self.web_elem = web_elem

    @property
    def name(self):
        css_sel = '.title-label'
        err_msg = 'no name found in displayed provider drop panel'
        header = find_web_elem(self.web_elem, css_sel, err_msg)
        return header.text

    @property
    def hostname(self):
        css_sel = '.provider-host-text'
        err_msg = 'no hostname found in displayed provider drop panel'
        header = find_web_elem(self.web_elem, css_sel, err_msg)
        return header.text

    def copy_hostname(self):
        css_sel = '.provider-host-copy-btn'
        err_msg = 'no copy hostname btn found in displayed provider drop panel'
        btn = find_web_elem(self.web_elem, css_sel, err_msg)
        btn.click()

    def go_to_your_files(self):
        css_sel = '.drop-body button'
        err_msg = "no 'Go to your files' btn found " \
                  "in displayed provider drop panel"
        btn = find_web_elem(self.web_elem, css_sel, err_msg)
        btn.click()

    @property
    def supported_spaces(self):
        css_sel = 'ul li.provider-place-drop-space'
        return (SpaceRecordInProvidersPanel(space,
                                            name_css='.space-label',
                                            size_css='.space-size')
                for space
                in self.web_elem.find_elements_by_css_selector(css_sel))

    def __getitem__(self, space_name):
        for space in self.supported_spaces:
            if space_name == space.name:
                return space
        else:
            raise RuntimeError('no supported space named "{space}" '
                               'for provider named "{provider}" in displayed '
                               'drop panel found'.format(provider=self.name,
                                                         space=space_name))
