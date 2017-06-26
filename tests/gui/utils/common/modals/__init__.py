"""Utils for operations on modals in GUI tests
"""

from .file_distribution import FileDistributionModal
from .add_storage import AddStorage
from tests.gui.utils.core.web_elements import WebItem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Modals(object):
    add_storage = WebItem('.panel-onezone-modal.in', cls=AddStorage)
    file_distribution = WebItem('#file-chunks-modal', cls=FileDistributionModal)

    def __init__(self, driver):
        self.driver = driver
        self.web_elem = driver

    def __str__(self):
        return 'modals'
