"""Steps used in clusters deployment process"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import re

from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, parse_seq, transform


@when(parsers.parse('user of {browser_id} sees that {options} options are '
                    'enabled for {host_regexp} host in Nodes page in Onepanel'))
@then(parsers.parse('user of {browser_id} sees that {options} options are '
                    'enabled for {host_regexp} host in Nodes page in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_options_enabled_for_host_in_nodes(selenium, browser_id, options,
                                                host_regexp, onepanel):
    options = [transform(option) for option in parse_seq(options)]
    err_msg = '{{}} not enabled for {host} in ' \
              'Nodes page in Onepanel'.format(host=host_regexp)
    for host in onepanel(selenium[browser_id]).content.nodes.hosts:
        if re.match(host_regexp, host.name):
            for option in options:
                toggle = getattr(host, option)
                assert toggle.is_checked(), err_msg.format(option)


@when(parsers.parse('user of {browser_id} sees that {options} options cannot '
                    'be changed for {host_regexp} host in Nodes page '
                    'in Onepanel'))
@then(parsers.parse('user of {browser_id} sees that {options} options cannot '
                    'be changed for {host_regexp} host in Nodes page '
                    'in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_options_enabled_for_host_in_nodes(selenium, browser_id, options,
                                                host_regexp, onepanel):
    options = [transform(option) for option in parse_seq(options)]
    err_msg = '{{}} can be changed for {host} in Nodes page in Onepanel, ' \
              'while it should not be'.format(host=host_regexp)
    for host in onepanel(selenium[browser_id]).content.nodes.hosts:
        if re.match(host_regexp, host.name):
            for option in options:
                toggle = getattr(host, option)
                assert not toggle.is_enabled(), err_msg.format(option)
