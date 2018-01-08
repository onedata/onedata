"""Utils and fixtures to facilitate operation on metadata row in file browser in oneprovider web GUI.
"""

from tests.gui.utils.core.common import PageObject
from tests.gui.utils.core.web_elements import InputWebElement, TextLabelWebElement, WebItem, WebItemsSequence, \
    ButtonWebItem, WebElementsSequence, WebElementsSequenceItemWithText
from tests.gui.utils.core.web_objects import ButtonWebObject
from tests.gui.utils.generic import click_on_web_elem, find_web_elem_with_text

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _BasicMetadataEntry(PageObject):
    attribute = id = TextLabelWebElement('th')
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

    def is_valid(self):
        return 'invalid' not in self.web_elem.get_attribute('class')


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


class _NavigationHeader(PageObject):
    _tabs = WebElementsSequence('li')
    basic = WebElementsSequenceItemWithText(seq=_tabs, text='BASIC',
                                            cls=ButtonWebObject)
    json = WebElementsSequenceItemWithText(seq=_tabs, text='JSON',
                                           cls=ButtonWebObject)
    rdf = WebElementsSequenceItemWithText(seq=_tabs, text='RDF',
                                          cls=ButtonWebObject)


class MetadataRow(PageObject):
    navigation = WebItem('ul.nav-tabs', cls=_NavigationHeader)

    basic = WebItem('table.metadata-basic-table', cls=_BasicMetadataPanel)
    json = WebItem('.metadata-json-editor', cls=_MetadataEditPanel)
    rdf = WebItem('.metadata-xml-editor', cls=_MetadataEditPanel)

    def __str__(self):
        return 'metadata row in {}'.format(self.parent)

    def save_all_changes(self):
        return self._get_btn('save all changes')

    def discard_changes(self):
        return self._get_btn('discard changes')

    def remove_metadata(self):
        return self._get_btn('remove metadata')

    def _get_btn(self, name):
        css_sel = '.save-metadata-row button'
        err_msg = '{} btn not found in metadata row'.format(name)
        btn = find_web_elem_with_text(self.web_elem, css_sel, name, err_msg)
        return ButtonWebObject(self.driver, btn, self)
