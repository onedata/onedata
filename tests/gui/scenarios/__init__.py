"""This package contains python tests to run GUI acceptance tests.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


import pytest


BROWSER = pytest.config.getoption('--driver')

try:
    pytest.config.getoption('--basic-url')
except ValueError:
    USING_BASE_URL = False
else:
    USING_BASE_URL = True
