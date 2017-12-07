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


def _assert_transfer(transfer, item_type, desc, sufix):
    assert getattr(transfer, 'is_{}'.format(item_type))(), \
        'Transferred item is not {} in {}'.format(item_type, sufix)
    
    desc = yaml.load(desc)
    status = desc.pop('status', None)
    if status:
        assert transfer.get_status() == status.lower(), \
            'Transfer status is {} instead of {} in {}'.format(transfer.
                                                               get_status(), 
                                                               status, sufix)
    for key, val in desc.items():
        transfer_val = getattr(transfer, key.replace(' ', '_'))
        assert transfer_val == str(val), \
            'Transfer {} is {} instead of {} in {}'.format(key, transfer_val, 
                                                           val, sufix)


@when(parsers.re('user of (?P<browser_id>.*) sees (?P<item_type>file|directory)'
                 ' in active transfers:\n(?P<desc>(.|\s)*)'))
@then(parsers.re('user of (?P<browser_id>.*) sees (?P<item_type>file|directory)'
                 ' in active transfers:\n(?P<desc>(.|\s)*)'))
@repeat_failed(interval=0.5)
def assert_active_transfer(selenium, browser_id, item_type, desc):
    transfer = op_page(selenium[browser_id]).transfers.active[0]
    _assert_transfer(transfer, item_type, desc, 'active transfers')


@when(parsers.re('user of (?P<browser_id>.*) sees (?P<item_type>file|directory)'
                 ' in history of transfers:\n(?P<desc>(.|\s)*)'))
@then(parsers.re('user of (?P<browser_id>.*) sees (?P<item_type>file|directory)'
                 ' in history of transfers:\n(?P<desc>(.|\s)*)'))
@repeat_failed(interval=0.5, timeout = 40)
def assert_finished_transfer(selenium, browser_id, item_type, desc):
    transfer = op_page(selenium[browser_id]).transfers.history[0]
    _assert_transfer(transfer, item_type, desc, 'history of transfers')


@when(parsers.re('user of (?P<browser_id>.*) waits for all transfers to finish'))
@then(parsers.re('user of (?P<browser_id>.*) waits for all transfers to finish'))
@repeat_failed(interval=1, timeout=60, exceptions=RuntimeError)
def wait_for_tranfers_to_finish(selenium, browser_id):
    try:
        _ = op_page(selenium[browser_id]).transfers.active[0]
    except RuntimeError as e:
        if not "Index out of bound" in e.args[0]:
            raise
    else:
        raise RuntimeError("Active transfer did not finish")


@when(parsers.re('user of (?P<browser_id>.*) expands first transfer record'))
@then(parsers.re('user of (?P<browser_id>.*) expands first transfer record'))
def expand_transfer_record(selenium, browser_id):
    op_page(selenium[browser_id]).transfers.history[0].expand()

    
@when(parsers.re('user of (?P<browser_id>.*) sees that there is non-zero '
                 'throughput in transfer chart'))
@then(parsers.re('user of (?P<browser_id>.*) sees that there is non-zero '
                 'throughput in transfer chart'))
def assert_non_zero_transfer_speed(selenium, browser_id):
    chart = op_page(selenium[browser_id]).transfers.history[0].get_chart()
    assert chart.get_speed() != '0', 'Transfer throughput is 0'


@when(parsers.re('user of (?P<browser_id>.*) migrates selected item from '
                 'provider "(?P<source>.*)" to provider "(?P<target>.*)"')) 
@then(parsers.re('user of (?P<browser_id>.*) migrates selected item from '
                 'provider "(?P<source>.*)" to provider "(?P<target>.*)"')) 
def migrate_item(selenium, browser_id, source, target):
    modal = modals(selenium[browser_id])
    modal.data_distribution.providers[source].migrate()
    modal.data_distribution.migrate[target].select()


@when(parsers.re('user of (?P<browser_id>.*) replicates selected item'
                 ' to provider "(?P<provider>.*)"'))    
@then(parsers.re('user of (?P<browser_id>.*) replicates selected item'
                 ' to provider "(?P<provider>.*)"'))    
def replicate_item(selenium, browser_id, provider):
    (modals(selenium[browser_id]).
            data_distribution.
            providers[provider].
            replicate())


@when(parsers.re('user of (?P<browser_id>.*) sees that item is never '
                 'synchronized in provider "(?P<provider>.*)"'))
@then(parsers.re('user of (?P<browser_id>.*) sees that item is never '
                 'synchronized in provider "(?P<provider>.*)"'))
def assert_item_never_synchronized(selenium, browser_id, provider):
    assert (modals(selenium[browser_id]).
                   data_distribution.
                   providers[provider].
                   distribution.
                   is_never_synchronized()), \
        "Item is synchronized in provider {}".format(provider)


@when(parsers.re('user of (?P<browser_id>.*) selects "(?P<space>.*)" space '
                 'in transfers tab'))
@then(parsers.re('user of (?P<browser_id>.*) selects "(?P<space>.*)" space '
                 'in transfers tab'))
def change_transfer_space(selenium, browser_id, space):
    op_page(selenium[browser_id]).transfers.spaces[space].select()
