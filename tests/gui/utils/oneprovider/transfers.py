"""Utils and fixtures to facilitate operations on transfers in Oneprovider GUI
"""

__author__ = "Michal Stanisz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.common import PageObject
from tests.gui.utils.oneprovider.data_tab.space_selector import SpaceRecord
from tests.gui.utils.core.web_elements import (TextLabelWebElement, WebElement,
                                               IconWebElement, WebItemsSequence,
                                               ButtonWithTextWebElement, WebItem)

TransferStatusList = ['completed', 'skipped', 'cancelled', 'failed', 'active', 
                      'invalidating', 'scheduled']
TransferTypeList = ['migration', 'replication', 'invalidation']


class TransferRecord(PageObject):
    name = TextLabelWebElement('td:first-of-type')
    username = TextLabelWebElement('td:nth-of-type(2)')
    destination = TextLabelWebElement('td:nth-of-type(3)')
    status_icon = IconWebElement('.cell-status')
    type_icon = IconWebElement('.cell-type')
    icon = IconWebElement('.transfer-file-icon')

    def __init__(self, driver, web_elem, parent, **kwargs):
        super(TransferRecord, self).__init__(driver, web_elem, parent, **kwargs)
        status_class = self.status_icon.get_attribute('class').split()
        type_class = self.type_icon.get_attribute('class').split()
        self.status = [x for x in status_class if x in TransferStatusList][0]
        self.type = [x for x in type_class if x in TransferTypeList][0]

    def get_chart(self):
        if not 'expanded-row' in self.web_elem.get_attribute('class'):
            raise RuntimeError("Transfer record for file {} is not expanded".
                                format(self.name))
        return TransferChart(self.driver, self.web_elem.find_element_by_xpath(
                             ' .//following-sibling::tr'), self.web_elem)

    def is_expanded(self):
        return "expanded-row" in self.web_elem.get_attribute('class')

    def expand(self):
        if not self.is_expanded():
            self.web_elem.click()

    def collapse(self):
        if self.is_expanded():
            self.web_elem.click()

    def is_file(self):
        return "oneicon-file" in self.icon.get_attribute('class') 

    def is_directory(self):
        return "oneicon-folder" in self.icon.get_attribute("class") 

    def __str__(self):
        return 'Transfer row {} in {}'.format(self.name, self.parent)


class TransferRecordHistory(TransferRecord):
    transferred = TextLabelWebElement('td:nth-of-type(6)')
    total_files = TextLabelWebElement('td:nth-of-type(7)')


class TransferRecordActive(TransferRecord):
    transferred = TextLabelWebElement('td:nth-of-type(5)')
    total_files = TextLabelWebElement('td:nth-of-type(6)')


class TransferChart(PageObject):
    minute = ButtonWithTextWebElement('button.btn-default', text = 'Minute')
    hour = ButtonWithTextWebElement('button.btn-default', text = 'Hour')
    active = TextLabelWebElement('button.btn-default.active')
    # We take only last point in the chart
    _speed = WebElement('.transfers-transfer-chart .ct-series line:last-of-type')

    def get_speed(self):
        return self._speed.get_attribute('ct:value').split(',')[1]


class TabHeader(PageObject):
    name = TextLabelWebElement('.nav-link')

    def click(self):
        self.web_elem.click()


class TransfersTab(PageObject):
    spaces = WebItemsSequence('ul.spaces-list li', cls=SpaceRecord)
    tabs = WebItemsSequence('.row-transfers-tables .nav-tabs li', 
                            cls = TabHeader)
    _history_list = WebItemsSequence('.col-completed-transfers tr.data-row', 
                                     cls = TransferRecordHistory)
    _active_list = WebItemsSequence('.col-active-transfers tr.data-row', 
                                    cls = TransferRecordActive)
    _scheduled_list = WebItemsSequence('.col-scheduled-transfers tr.data-row', 
                                       cls = TransferRecordHistory)

    @property
    def active(self):
        self['active'].click()
        return self._active_list

    @property
    def history(self):
        self['history'].click()
        return self._history_list
    
    @property
    def scheduled(self):
        self['scheduled'].click()
        return self._scheduled_list

    def __getitem__(self, name):
        for tab in self.tabs:
            if name in tab.name.lower():
                return tab
        else:
            raise RuntimeError('no tab named {} in transfer tab'.format(name))
