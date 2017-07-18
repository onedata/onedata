"""Utils and fixtures to facilitate operation on metadata row
in file browser in oneprovider web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (Input, Label, WebItem,
                                               WebItemsSequence, Button,
                                               NamedButton)
from tests.gui.utils.core.web_objects import ButtonPageObject
from tests.gui.utils.generic import find_web_elem_with_text


class BasicMetadataEntry(PageObject):
    attribute = id = Label('th')
    value = Label('td')
    remove = Button('.oneicon-close')

    def __str__(self):
        return 'metadata basic entry'


class BasicMetadataNewEntry(PageObject):
    attribute = Input('th input[placeholder=Attribute]')
    value = Input('td input[placeholder=Value]')
    add = Button('.oneicon-add')

    def is_valid(self):
        return 'invalid' not in self.web_elem.get_attribute('class')


class BasicMetadataPanel(PageObject):
    new_entry = WebItem('tr.basic-new-entry', cls=BasicMetadataNewEntry)
    entries = WebItemsSequence('tr:not([class~=basic-new-entry])',
                               cls=BasicMetadataEntry)


class MetadataEditPanel(PageObject):
    text_area = Input('textarea')
    status = Label('.parse-status-panel')


class NavigationHeader(PageObject):
    basic = NamedButton('li', text='BASIC')
    json = NamedButton('li', text='JSON')
    rdf = NamedButton('li', text='RDF')


class MetadataRow(PageObject):
    navigation = WebItem('ul.nav-tabs', cls=NavigationHeader)

    basic = WebItem('table.metadata-basic-table', cls=BasicMetadataPanel)
    json = WebItem('.metadata-json-editor', cls=MetadataEditPanel)
    rdf = WebItem('.metadata-xml-editor', cls=MetadataEditPanel)

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
        return ButtonPageObject(self.driver, btn, self)
