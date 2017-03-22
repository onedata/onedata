"""Utils and fixtures to facilitate operation on metadata row in file browser in oneprovider web GUI.
"""

from tests.gui.utils.core.common import PageObject
from tests.gui.utils.core.web_elements import InputWebElement, TextLabelWebElement, WebItem, WebItemsSequence, \
    ButtonWebItem
from tests.gui.utils.generic import click_on_web_elem, find_web_elem, \
    find_web_elem_with_text

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _BasicMetadataEntry(PageObject):
    attribute = TextLabelWebElement('th')
    value = TextLabelWebElement('td')
    remove = ButtonWebItem('.oneicon-close')

    def __str__(self):
        return 'metadata basic entry'


class _BasicMetadataNewEntry(PageObject):
    attribute = InputWebElement('th input[placeholder=Attribute]')
    value = InputWebElement('td input[placeholder=Value]')
    add = ButtonWebItem('.oneicon-add')

    def __str__(self):
        return 'metadata basic new entry in {}'.format(self.parent)


class _BasicMetadataPanel(PageObject):
    new_entry = WebItem('tr.basic-new-entry', cls=_BasicMetadataNewEntry)
    entries = WebItemsSequence('tr:not([class~=basic-new-entry])',
                               cls=_BasicMetadataEntry)

    def __str__(self):
        return 'basic metadata panel in {}'.format(self.parent)


class _MetadataEditPanel(PageObject):
    text_area = InputWebElement('textarea')
    status = TextLabelWebElement('.parse-status-panel')

    def __str__(self):
        return 'metadata edit panel in {}'.format(self.parent)


class MetadataRow(PageObject):
    __buttons__ = 'asd'

    basic = WebItem('table.metadata-basic-table', cls=_BasicMetadataPanel)
    json = WebItem('.metadata-json-editor', cls=_MetadataEditPanel)
    rdf = WebItem('.metadata-xml-editor', cls=_MetadataEditPanel)

    def __str__(self):
        return 'metadata row in {}'.format(self.parent)

    @property
    def json(self):
        self._change_tab('json')
        css_sel = '.metadata-json-editor'
        err_msg = 'edit panel for JSON not found in metadata row'
        return _MetadataEditPanel(self.driver,
                                  find_web_elem(self.web_elem, css_sel, err_msg),
                                  'JSON')

    @property
    def rdf(self):
        self._change_tab('rdf')
        css_sel = '.metadata-xml-editor'
        err_msg = 'edit panel for RDF not found in metadata row'
        return _MetadataEditPanel(self.driver,
                                  find_web_elem(self.web_elem, css_sel, err_msg),
                                  'RDF')

    def _change_tab(self, name):
        css_sel = 'ul.nav-tabs li'
        err_msg = '{} tab not found in metadata row'.format(name)
        tab = find_web_elem_with_text(self.web_elem, css_sel, name, err_msg)
        if 'active' not in tab.get_attribute('class'):
            click_on_web_elem(self.driver, tab, 'clicking on {} tab in metadata '
                                                'row disabled'.format(name))

    def save_changes(self):
        self._click_btn('save all changes')

    def discard_changes(self):
        self._click_btn('discard changes')

    def remove_metadata(self):
        self._click_btn('remove metadata')

    def _click_btn(self, name):
        css_sel = '.save-metadata-row button'
        err_msg = '{} btn not found in metadata row'.format(name)
        btn = find_web_elem_with_text(self.web_elem, css_sel, name, err_msg)

        err_msg = 'clicking on {} btn in metadata row disabled'.format(name)
        click_on_web_elem(self.driver, btn, err_msg)
