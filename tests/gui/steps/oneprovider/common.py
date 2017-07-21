"""Steps used for common operations in Oneprovider GUI"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


import time

from pytest_bdd import given, parsers, when, then

from tests.gui.utils.generic import parse_seq, repeat_failed
from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.utils.generic import parse_url


def _wait_for_op_session_to_start(selenium, browser_id_list):
    # TODO rm *4 when provider session starts becomes faster
    @repeat_failed(timeout=WAIT_BACKEND*4)
    def _assert_correct_url(d):
        try:
            found = parse_url(d.current_url).group('access')
        except AttributeError:
            raise RuntimeError('no access part found in url')
        else:
            if 'onedata' != found.lower():
                raise RuntimeError('expected onedata as access part in url '
                                   'instead got: {}'.format(found))

    for browser_id in parse_seq(browser_id_list):
        driver = selenium[browser_id]

        # because of current subscription it is necessary
        # to wait under certain circumstances for things to properly work
        time.sleep(12)
        driver.get(parse_url(driver.current_url).group('base_url'))

        _assert_correct_url(driver)


@given(parsers.re('users? of (?P<browser_id_list>.*?) seen that '
                  'Oneprovider session has started'))
def g_wait_for_op_session_to_start(selenium, browser_id_list):
    _wait_for_op_session_to_start(selenium, browser_id_list)


@when(parsers.re('users? of (?P<browser_id_list>.*?) sees that '
                 'Oneprovider session has started'))
@then(parsers.re('users? of (?P<browser_id_list>.*?) sees that '
                 'Oneprovider session has started'))
def wt_wait_for_op_session_to_start(selenium, browser_id_list):
    _wait_for_op_session_to_start(selenium, browser_id_list)


@when(parsers.parse('user of {browser_id} sees that provider name displayed in '
                    'Oneprovider page has value of "{val}"'))
@then(parsers.parse('user of {browser_id} sees that provider name displayed in '
                    'Oneprovider page has value of "{val}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_provider_name_in_op(selenium, browser_id, val, op_page):
    displayed_name = op_page(selenium[browser_id]).provider_name
    assert displayed_name == val, \
        ('displayed {} provider name in Oneprovider GUI instead of '
         'expected {}'.format(displayed_name, val))
