"""Utils and fixtures to facilitate operations on DATA SPACE MANAGEMENT panel
in Onezone web GUI.
"""

from tests.gui.utils.generic import iter_ahead, find_web_elem, find_web_elem_with_text
from tests.gui.utils.onezone.sidebar_panel_record import OZPanelRecord
from tests.gui.utils.onezone.sidebar_panel import OZPanel
from tests.gui.utils.onezone.edit_box import EditBox


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class DataSpaceManagementPanel(OZPanel):

    @property
    def spaces(self):
        css_sel = '.spaces-accordion-item'
        return [SpaceRecord(space, record_type='space',
                            submenu_toggle='.clickable') for space in
                self.web_elem.find_elements_by_css_selector(css_sel)]

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


class SpaceRecord(OZPanelRecord):

    class ProviderRecord(object):
        def __init__(self, web_elem):
            self.web_elem = web_elem

        def __eq__(self, other):
            if isinstance(other, str) or isinstance(other, unicode):
                return self.name == other
            else:
                raise NotImplementedError('operation not implemented')

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
            return [SpaceRecord.ProviderRecord(provider) for provider in
                    self.web_elem.find_elements_by_css_selector(css_sel)]
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
        css_sel = '.space-header-size'
        err_msg = 'no size label for space named "{}" found in ' \
                  'DATA SPACE MANAGEMENT oz panel'.format(self.name)
        size_label = find_web_elem(self.web_elem, css_sel, err_msg)
        return size_label.text

    @property
    def providers_count(self):
        css_sel = '.providers-count'
        err_msg = 'no count for supporting providers ' \
                  'for space named "{}" found in DATA SPACE MANAGEMENT ' \
                  'oz panel'.format(self.name)
        count_label = find_web_elem(self.web_elem, css_sel, err_msg)
        return int(count_label.text)

    class SettingsDropdown(object):
        def __init__(self, web_elem):
            self.web_elem = web_elem

        @property
        def is_expanded(self):
            css_class = self.web_elem.get_attribute('class')
            return True if (css_class and 'open' in css_class) else False

        def expand(self):
            if not self.is_expanded:
                self.web_elem.click()

        def collapse(self):
            if self.is_expanded:
                self.web_elem.click()

        def set_as_home(self):
            btn = self._get_btn('set as home')
            btn.click()

        def rename(self):
            btn = self._get_btn('rename')
            btn.click()

        def get_support(self):
            btn = self._get_btn('get support')
            btn.click()

        def leave(self):
            btn = self._get_btn('leave')
            btn.click()

        def _get_btn(self, name):
            css_sel = 'ul.space-dropdown-menu li.clickable'
            err_msg = 'no button named {btn} found in settings ' \
                      'dropdown for given space in DATA SPACE ' \
                      'MANAGAMENT oz panel'.format(btn=name)
            return find_web_elem_with_text(self.web_elem, css_sel,
                                           name, err_msg)

    @property
    def settings(self):
        css_sel = '.settings-tool .settings-dropdown'
        err_msg = 'no settings icon for space named "{}" in DATA SPACE ' \
                  'MANAGEMENT oz pnale found'.format(self.name)
        settings = find_web_elem(self.web_elem, css_sel, err_msg)
        return SpaceRecord.SettingsDropdown(settings)

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

        def copy_token(self):
            css_sel = 'button'
            err_msg = 'no copy button found for ' \
                      'given space support token dropdown'
            btn = find_web_elem(self.web_elem, css_sel, err_msg)
            btn.click()

    @property
    def dropright_with_token(self):
        css_sel = 'ul.tertiary-list li.get-support .dropdown-menu'
        err_msg = 'no dropdown menu with support token for "{}" space found ' \
                  'in DATA SPACE MANAGEMENT'.format(self.name)
        dropright = find_web_elem(self.web_elem, css_sel, err_msg)
        return SpaceRecord.SpaceSupportTokenDropdownMenu(dropright)

    def get_support(self):
        if self.is_submenu_expanded:
            space_name = self.name
            css_sel = 'ul.tertiary-list li.get-support .dropdown'
            err_msg = 'no get support btn found for "{}" space'.format(space_name)
            btn = find_web_elem(self.web_elem, css_sel, err_msg)
            btn.click()
        else:
            raise RuntimeError('submenu for space named "{}" in DATA SPACE '
                               'MANAGEMENT is not expanded'.format(self.name))
