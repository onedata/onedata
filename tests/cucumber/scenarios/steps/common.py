"""Author: Piotr Ociepka
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements some common basic functions and functionality.
"""

import pytest


class Context:
    def __init__(self):
        pass


@pytest.fixture(scope="module")
def context():
    return Context()
