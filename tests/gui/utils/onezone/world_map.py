"""Utils and fixtures to facilitate operations on world map in Onezone web GUI.
"""

from selenium.common.exceptions import NoSuchElementException

from tests.gui.utils.generic import find_web_elem, click_on_web_elem
from tests.gui.utils.onezone.providers import SpaceRecordInProvidersPanel


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class WorldMap(object):

    def __init__(self, web_elem, driver):
        self.web_elem = web_elem
        self._driver = driver

    def click(self):
        err_msg = 'clicking on world map disabled'
        click_on_web_elem(self._driver, self.web_elem, err_msg)

    @property
    def message(self):
        css_sel = '.panel-onezone-alert'
        err_msg = 'no alert on world map in oz found'
        alert = find_web_elem(self.web_elem, css_sel, err_msg)
        return Message(alert)

    @property
    def providers(self):
        css_sel = '.provider-place'
        return [ProviderPopup(provider, self._driver) for provider in
                self.web_elem.find_elements_by_css_selector(css_sel)]

    def __getitem__(self, index):
        providers = self.providers
        try:
            provider = providers[index]
        except IndexError:
            raise RuntimeError('asked for {index} provider but there are only '
                               '{num} providers on map'.format(index=index,
                                                               num=len(providers)))
        else:
            return provider

    def get_provider_with_displayed_panel(self):
        for provider in self.providers:
            if provider.is_displayed:
                return provider


class ProviderPopup(object):

    def __init__(self, web_elem, driver):
        self.web_elem = web_elem
        self._driver = driver

    def click(self):
        err_msg = 'clickin on provider popup for "{}" disabled'.format(self.name)
        click_on_web_elem(self._driver, self.web_elem, err_msg)

    @property
    def is_working(self):
        return 'working' in self.web_elem.get_attribute('class')

    @property
    def is_displayed(self):
        css_sel = '.provider-place-drop'
        try:
            self.web_elem.find_element_by_css_selector(css_sel)
        except NoSuchElementException:
            return False
        else:
            return True

    @property
    def name(self):
        if self.is_displayed:
            css_sel = '.title-label'
            err_msg = 'no name found in displayed provider drop panel'
            header = find_web_elem(self.web_elem, css_sel, err_msg)
            return header.text
        else:
            raise RuntimeError('no displayed panel found for given provider')

    @property
    def hostname(self):
        if self.is_displayed:
            css_sel = 'input.provider-host-text'
            err_msg = 'no hostname found in displayed provider drop panel'
            header = find_web_elem(self.web_elem, css_sel, err_msg)
            return header.get_attribute('value')
        else:
            raise RuntimeError('no displayed panel found for given provider')

    def copy_hostname(self):
        if self.is_displayed:
            css_sel = '.provider-host-copy-btn'
            err_msg = 'no copy hostname btn found in displayed provider drop panel'
            btn = find_web_elem(self.web_elem, css_sel, err_msg)
            err_msg = 'clicking on cp hostname btn in provider "{}" ' \
                      'popup disabled'.format(self.name)
            click_on_web_elem(self._driver, btn, err_msg)
        else:
            raise RuntimeError('no displayed panel found for given provider')

    def go_to_your_files(self):
        if self.is_displayed:
            css_sel = '.drop-body button'
            err_msg = "no 'Go to your files' btn found " \
                      "in displayed provider drop panel"
            btn = find_web_elem(self.web_elem, css_sel, err_msg)
            err_msg = 'clicking on "Go to your files" btn in provider "{}" ' \
                      'popup disabled'.format(self.name)
            click_on_web_elem(self._driver, btn, err_msg)
        else:
            raise RuntimeError('no displayed panel found for given provider')

    @property
    def supported_spaces(self):
        if self.is_displayed:
            css_sel = 'ul li.provider-place-drop-space'
            return [SpaceRecordInProvidersPanel(space,
                                                name_css='.space-label',
                                                size_css='.space-size')
                    for space
                    in self.web_elem.find_elements_by_css_selector(css_sel)]
        else:
            raise RuntimeError('no displayed panel found for given provider')

    def __getitem__(self, space_name):
        if self.is_displayed:
            for space in self.supported_spaces:
                if space_name == space.name:
                    return space
            else:
                raise RuntimeError('no supported space named "{space}" '
                                   'for provider named "{provider}" in displayed '
                                   'drop panel found'.format(provider=self.name,
                                                             space=space_name))
        else:
            raise RuntimeError('no displayed panel found for given provider')


class Message(object):

    def __init__(self, web_elem):
        self.web_elem = web_elem

    @property
    def title(self):
        css_sel = '.panel-heading'
        err_msg = 'no heading for alert on world map in oz found'
        title = find_web_elem(self.web_elem, css_sel, err_msg)
        return title.text

    @property
    def msg(self):
        css_sel = '.panel-body'
        err_msg = 'no msg in alert on world map in oz found'
        msg = find_web_elem(self.web_elem, css_sel, err_msg)
        return msg.text
