"""Utils and fixtures to facilitate operation on metadata row in file browser in oneprovider web GUI.
"""

from tests.gui.utils.common.web_elements import InputWebElement, TextLabelWebElement
from tests.gui.utils.generic import click_on_web_elem, find_web_elem, \
    find_web_elem_with_text

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class MetadataEditPanel(object):
    text_area = InputWebElement(name='text_area', css_sel='textarea')
    status = TextLabelWebElement(name='status', css_sel='.parse-status-panel')

    def __init__(self, driver, web_elem, tab):
        self.web_elem = web_elem
        self._driver = driver
        self._tab = tab

    def __str__(self):
        return 'metadata edit panel'


class MetadataBasicEntry(object):
    attribute = TextLabelWebElement(name='attribute', css_sel='th')
    value = TextLabelWebElement(name='value', css_sel='td')

    def __init__(self, driver, web_elem):
        self.web_elem = web_elem
        self._driver = driver

    def __str__(self):
        return 'metadata basic entry'

    def remove(self):
        css_sel = '.oneicon-close'
        err_msg = 'unable to locate rm btn for given metadata basic entry'
        btn = find_web_elem(self.web_elem, css_sel, err_msg)

        err_msg = 'clicking on rm button for given metadata basic entry disabled'
        click_on_web_elem(self._driver, btn, err_msg)


class MetadataNewBasicEntry(object):
    attribute = InputWebElement(name='attribute',
                                css_sel='th input[placeholder=Attribute]')
    value = InputWebElement(name='value',
                            css_sel='td input[placeholder=Value]')

    def __init__(self, driver, web_elem):
        self.web_elem = web_elem
        self._driver = driver

    def __str__(self):
        return 'metadata basic new entry'

    def add(self):
        css_sel = '.oneicon-add'
        err_msg = 'unable to locate add btn for new metadata basic entry'
        btn = find_web_elem(self.web_elem, css_sel, err_msg)

        err_msg = 'clicking on add button for new metadata basic entry disabled'
        click_on_web_elem(self._driver, btn, err_msg)


class MetadataBasicEditPanel(object):
    def __init__(self, driver, web_elem, parent):
        self.web_elem = web_elem
        self._driver = driver

    @property
    def entries_count(self):
        return len(self._get_entries())

    @property
    def entries(self):
        return [MetadataBasicEntry(self._driver, item)
                for item in self._get_entries()]

    def __getitem__(self, item):
        items_count = self.entries_count
        if item >= items_count:
            raise RuntimeError('requested index {index} out of bound '
                               '{limit} for basic metadata '
                               'entry'.format(index=item, limit=items_count))
        else:
            return MetadataBasicEntry(self._driver, self._get_entries()[item])

    def _get_entries(self):
        css_sel = 'tr:not([class~=basic-new-entry])'
        return self.web_elem.find_elements_by_css_selector(css_sel)

    @property
    def new_entry(self):
        css_sel = 'tr.basic-new-entry'
        err_msg = 'unable to locate new entry row for given basic metadata row'
        entry = find_web_elem(self.web_elem, css_sel, err_msg)
        return MetadataNewBasicEntry(self._driver, entry)


class MetadataRow(object):
    def __init__(self, driver, web_elem, parent):
        self.web_elem = web_elem
        self._driver = driver

    # TODO implement
    @property
    def basic(self):
        self._change_tab('basic')
        css_sel = 'table.metadata-basic-table'
        err_msg = 'edit panel for BASIC not found in metadata row'
        panel = find_web_elem(self.web_elem, css_sel, err_msg)
        return MetadataBasicEditPanel(self._driver, panel, self)

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
        css_sel = 'ul.nav-tabs li'
        err_msg = '{} tab not found in metadata row'.format(name)
        tab = find_web_elem_with_text(self.web_elem, css_sel, name, err_msg)
        if 'active' not in tab.get_attribute('class'):
            click_on_web_elem(self._driver, tab, 'clicking on {} tab in metadata '
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
        click_on_web_elem(self._driver, btn, err_msg)
