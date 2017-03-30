"""Utils for operations on modals in GUI tests
"""

from tests.gui.utils.common.modals.file_distribution import FileDistributionModal
from tests.gui.utils.core.web_elements import WebItem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Modals(object):
    file_distribution = WebItem('#file-chunks-modal', cls=FileDistributionModal)

    def __init__(self, driver):
        self.driver = driver
        self.web_elem = driver

    def __str__(self):
        return 'modals'
