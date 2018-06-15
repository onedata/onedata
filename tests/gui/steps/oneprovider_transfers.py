"""Steps for tests of Oneprovider transfers
"""

__author__ = "Michal Stanisz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import yaml
from pytest_bdd import when, then, parsers
from tests.gui.utils.oneprovider_gui import OPLoggedIn as op_page
from tests.gui.utils.common.modals import Modals as modals
from tests.gui.utils.generic import repeat_failed
from selenium.common.exceptions import StaleElementReferenceException
from functools import partial


def _assert_transfer(transfer, item_type, desc, sufix):
    assert getattr(transfer, 'is_{}'.format(item_type))(), \
        'Transferred item is not {} in {}'.format(item_type, sufix)

    desc = yaml.load(desc)
    for key, val in desc.items():
        transfer_val = getattr(transfer, key.replace(' ', '_'))
        assert transfer_val == str(val), \
            'Transfer {} is {} instead of {} in {}'.format(key, transfer_val, 
                                                           val, sufix)


def wt(name, func=None, converters=None):
    if not func:
        return partial(wt, name, converters=converters)
    return then(name, converters)(when(name, converters)(func))


@wt(parsers.re('user of (?P<browser_id>.*) sees (?P<item_type>file|directory)'
               ' in ongoing transfers:\n(?P<desc>(.|\s)*)'))
@repeat_failed(interval=0.5)
def assert_ongoing_transfer(selenium, browser_id, item_type, desc):
    transfer = op_page(selenium[browser_id]).transfers.ongoing[0]
    _assert_transfer(transfer, item_type, desc, 'ongoing')


@wt(parsers.re('user of (?P<browser_id>.*) sees (?P<item_type>file|directory)'
               ' in ended transfers:\n(?P<desc>(.|\s)*)'))
@repeat_failed(interval=0.5, timeout = 40)
def assert_finished_transfer(selenium, browser_id, item_type, desc):
    transfer = op_page(selenium[browser_id]).transfers.ended[0]
    _assert_transfer(transfer, item_type, desc, 'ended')


@wt(parsers.re('user of (?P<browser_id>.*) sees (?P<item_type>file|directory)'
               ' in waiting transfers:\n(?P<desc>(.|\s)*)'))
@repeat_failed(interval=0.5, timeout = 40)
def assert_waiting_transfer(selenium, browser_id, item_type, desc):
    transfer = op_page(selenium[browser_id]).transfers.waiting[0]
    _assert_transfer(transfer, item_type, desc, 'waiting')


@wt(parsers.re('user of (?P<browser_id>.*) waits for all transfers to start'))
@repeat_failed(interval = 1, timeout = 90, 
               exceptions = (AssertionError, StaleElementReferenceException))
def wait_for_waiting_tranfers_to_start(selenium, browser_id):
    assert len(op_page(selenium[browser_id]).transfers.waiting) == 0, \
            'Waiting transfers did not start'


@wt(parsers.re('user of (?P<browser_id>.*) waits for all transfers to finish'))
@repeat_failed(interval = 1, timeout = 90, 
               exceptions = (AssertionError, StaleElementReferenceException))
def wait_for_ongoing_tranfers_to_finish(selenium, browser_id):
    assert len(op_page(selenium[browser_id]).transfers.ongoing) == 0, \
            'Active transfers did not finish'


@wt(parsers.re('user of (?P<browser_id>.*) expands first transfer record'))
def expand_transfer_record(selenium, browser_id):
    op_page(selenium[browser_id]).transfers.ended[0].expand()

    
@wt(parsers.re('user of (?P<browser_id>.*) sees that there is non-zero '
               'throughput in transfer chart'))
def assert_non_zero_transfer_speed(selenium, browser_id):
    chart = op_page(selenium[browser_id]).transfers.ended[0].get_chart()
    assert chart.get_speed() != '0', 'Transfer throughput is 0'


@wt(parsers.re('user of (?P<browser_id>.*) migrates selected item from '
               'provider "(?P<source>.*)" to provider "(?P<target>.*)"')) 
def migrate_item(selenium, browser_id, source, target):
    modal = modals(selenium[browser_id])
    modal.data_distribution.providers[source].migrate()
    modal.data_distribution.migrate[target].select()


@wt(parsers.re('user of (?P<browser_id>.*) replicates selected item'
               ' to provider "(?P<provider>.*)"'))    
def replicate_item(selenium, browser_id, provider):
    (modals(selenium[browser_id]).
            data_distribution.
            providers[provider].
            replicate())


@wt(parsers.re('user of (?P<browser_id>.*) sees that item is never '
               'synchronized in provider "(?P<provider>.*)"'))
def assert_item_never_synchronized(selenium, browser_id, provider):
    assert (modals(selenium[browser_id]).
                   data_distribution.
                   providers[provider].
                   distribution.
                   is_never_synchronized()), \
        "Item is synchronized in provider {}".format(provider)


@wt(parsers.re('user of (?P<browser_id>.*) selects "(?P<space>.*)" space '
               'in transfers tab'))
def change_transfer_space(selenium, browser_id, space):
    op_page(selenium[browser_id]).transfers.spaces[space].select()
