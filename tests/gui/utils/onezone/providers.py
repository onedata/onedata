"""Utils and fixtures to facilitate operations on world map in Onezone web GUI.
"""

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.generic import find_web_elem
from tests.gui.utils.onezone.sidebar_panel import OZPanel
from tests.gui.utils.onezone.sidebar_panel_record import OZPanelRecord

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class GoToYourFilesPanel(OZPanel):

    @property
    def providers(self):
        css_sel = '#providers-list .providers-accordion-item'
        return [ProviderRecord(provider, 'provider', '.spaces-count')
                for provider
                in self.web_elem.find_elements_by_css_selector(css_sel)]

    def __getitem__(self, name):
        for provider in self.providers:
            if name == provider.name:
                return provider
        else:
            raise RuntimeError('no provider named "{prov}" found in {panel} '
                               'oz panel'.format(prov=name, panel=self.name))


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

    def click(self):
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
            return [SpaceRecordInProvidersPanel(space,
                                                name_css='.one-label.truncate',
                                                size_css='.space-header-size')
                    for space
                    in self.web_elem.find_elements_by_css_selector(css_sel)]
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


class SpaceRecordInProvidersPanel(object):
    def __init__(self, web_elem, name_css, size_css):
        self.web_elem = web_elem
        self.name_css = name_css
        self.size_css = size_css

    def __eq__(self, other):
        if isinstance(other, str) or isinstance(other, unicode):
            return self.name == other
        else:
            raise NotImplementedError('operation not implemented')

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
