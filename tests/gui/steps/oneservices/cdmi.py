"""Steps for cdmi usage.
"""

from pytest_bdd import parsers, then, when

from tests.gui.conftest import WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} writes "{text}" to "{path}" '
                    'starting at offset {offset:d} in "{provider}" provider '
                    'using cdmi api'))
@then(parsers.parse('user of {browser_id} writes "{text}" to "{path}" '
                    'starting at offset {offset:d} in "{provider}" provider '
                    'using cdmi api'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def partial_upload_to_file_using_cdmi(browser_id, text, path, offset,
                                      provider, cdmi, tmp_memory):
    client = cdmi(tmp_memory[provider], tmp_memory[browser_id]['access_token'])
    client.write_to_file(path, text, offset)


@when(parsers.parse('user of {browser_id} reads from "{path}" in range '
                    '{start:d} to {end:d} in "{provider}" provider '
                    'using cdmi api'))
@then(parsers.parse('user of {browser_id} reads from "{path}" in range '
                    '{start:d} to {end:d} in "{provider}" provider '
                    'using cdmi api'))
@repeat_failed(attempts=WAIT_BACKEND, timeout=True)
def partial_read_from_file_using_cdmi(browser_id, path, start, end,
                                      provider, cdmi, tmp_memory):
    client = cdmi(tmp_memory[provider], tmp_memory[browser_id]['access_token'])
    print client.read_from_file(path, read_range=(start, end))
