"""Utils for operations on modals in GUI tests
"""

from tests.gui.utils.common.modals.file_distribution import FileDistributionModal
from tests.gui.utils.common.web_elements import WebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Modals(object):
    file_distribution = WebElement('#file-chunks-modal', cls=FileDistributionModal)

    def __init__(self, driver):
        self._driver = driver
        self.web_elem = driver

    def __str__(self):
        return 'modals'
