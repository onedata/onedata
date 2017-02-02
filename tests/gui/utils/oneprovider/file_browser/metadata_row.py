"""Utils and fixtures to facilitate operation on metadata row in file browser in oneprovider web GUI.
"""

from tests.gui.utils.generic import click_on_web_elem, find_web_elem, repeat_failed

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class MetadataEditPanel(object):
    def __init__(self, driver, web_elem, tab):
        self.web_elem = web_elem
        self._driver = driver
        self._tab = tab

    @property
    def textarea(self):
        return self._get_textarea().get_attribute('value')

    @textarea.setter
    @repeat_failed(attempts=10)
    def textarea(self, text):
        area = self._get_textarea()
        area.clear()
        area.send_keys(text)
        assert self.textarea == text, 'entering {} to textarea ' \
                                      'failed'.format(text)

    def _get_textarea(self):
        css_sel = 'textarea'
        err_msg = 'unable to locate textarea in {} metadata ' \
                  'row'.format(self._tab)
        return find_web_elem(self.web_elem, css_sel, err_msg)

    @property
    def status(self):
        css_sel = '.parse-status-panel'
        err_msg = 'unable to locate status panel in {} metadata ' \
                  'row'.format(self._tab)
        return find_web_elem(self.web_elem, css_sel, err_msg).text


class MetadataRow(object):
    def __init__(self, driver, web_elem):
        self.web_elem = web_elem
        self._driver = driver

    class MetadataBasicEditPanel(object):
        def __init__(self, driver, web_elem):
            self.web_elem = web_elem
            self._driver = driver

    # TODO implement
    @property
    def basic(self):
        self._change_tab('basic')
        css_sel = 'table.metadata-basic-table'

    @property
    def json(self):
        self._change_tab('json')
        css_sel = '.metadata-json-editor'
        err_msg = 'edit panel for JSON not found in metadata row'
        return MetadataEditPanel(self._driver,
                                 find_web_elem(self.web_elem, css_sel, err_msg),
                                 'JSON')

    @property
    def rdf(self):
        self._change_tab('rdf')
        css_sel = '.metadata-xml-editor'
        err_msg = 'edit panel for RDF not found in metadata row'
        return MetadataEditPanel(self._driver,
                                 find_web_elem(self.web_elem, css_sel, err_msg),
                                 'RDF')

    def _change_tab(self, name):
        css_sel = 'ul.nav-tabs li a'
        for item in self.web_elem.find_elements_by_css_selector(css_sel):
            if item.text.lower() == name.lower():
                click_on_web_elem(self._driver, item, 'clicking on {} tab in '
                                                      'metadata row '
                                                      'disabled'.format(name))
                break
        else:
            raise RuntimeError('{} tab not found in metadata row'.format(name))

    def save_changes(self):
        self._click_btn('save all changes')

    def discard_changes(self):
        self._click_btn('discard changes')

    def remove_metadata(self):
        self._click_btn('remove metadata')

    def _click_btn(self, name):
        css_sel = '.save-metadata-row button'
        err_msg = 'clicking on {} btn in metadata row disabled'
        for btn in self.web_elem.find_elements_by_css_selector(css_sel):
            if btn.text.lower() == name.lower():
                click_on_web_elem(self._driver, btn, err_msg.format(name))
                break
        else:
            raise RuntimeError('{} btn not found in metadata row'.format(name))
