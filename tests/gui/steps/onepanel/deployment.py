"""Steps used in clusters deployment process"""

import re
import time

from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_FRONTEND, SELENIUM_IMPLICIT_WAIT
from tests.gui.utils.generic import (repeat_failed, parse_seq,
                                     implicit_wait, transform)


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} enables {options} options for '
                    '{host_regexp} host in step 1 of deployment process '
                    'in Onepanel'))
@then(parsers.parse('user of {browser_id} enables {options} options for '
                    '{host_regexp} host in step 1 of deployment process '
                    'in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_check_host_options_in_deployment_step1(selenium, browser_id, options,
                                              host_regexp, onepanel):
    options = [transform(option) for option in parse_seq(options)]
    for host in onepanel(selenium[browser_id]).content.deployment.step1.hosts:
        if re.match(host_regexp, host.name):
            for option in options:
                toggle = getattr(host, option)
                toggle.check()


@when(parsers.parse('user of {browser_id} types ip address of "{host}" zone '
                    'to Onezone domain field in step 2 of deployment process '
                    'in Onepanel'))
@then(parsers.parse('user of {browser_id} types ip address of "{host}" zone '
                    'to Onezone domain field in step 2 of deployment process '
                    'in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_zone_ip_to_zone_domain_in_deployment_step2(selenium, browser_id,
                                                       host, hosts, onepanel):
    step = onepanel(selenium[browser_id]).content.deployment.step2
    step.oz_domain = hosts['onezone'][host]


@when(parsers.parse('user of {browser_id} types redirection point of "{host}" '
                    'provider to Redirection point field in step 2 of '
                    'deployment process in Onepanel'))
@then(parsers.parse('user of {browser_id} types redirection point of "{host}" '
                    'provider to Redirection point field in step 2 of '
                    'deployment process in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_enter_redirection_point_in_deployment_step2(selenium, browser_id,
                                                   host, hosts, onepanel):
    step = onepanel(selenium[browser_id]).content.deployment.step2
    step.redirection_point = 'https://{}'.format(hosts['oneprovider'][host])


@when(parsers.re('user of (?P<browser_id>.+?) types "(?P<text>.+?)" to '
                 '(?P<input_box>.+?) field in '
                 '(?P<step>step 1|step 2|step 3|last step) of deployment '
                 'process in Onepanel'))
@then(parsers.re('user of (?P<browser_id>.+?) types "(?P<text>.+?)" to '
                 '(?P<input_box>.+?) field in '
                 '(?P<step>step 1|step 2|step 3|last step) of deployment '
                 'process in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_text_to_in_box_in_deployment_step(selenium, browser_id, text,
                                              input_box, step, onepanel):
    step = getattr(onepanel(selenium[browser_id]).content.deployment,
                   step.replace(' ', ''))
    setattr(step, transform(input_box), text)


@when(parsers.re('user of (?P<browser_id>.+?) clicks on (?P<btn>.+?) button '
                 'in (?P<step>step 1|step 2|step 3|last step) of deployment '
                 'process in Onepanel'))
@then(parsers.re('user of (?P<browser_id>.+?) clicks on (?P<btn>.+?) button '
                 'in (?P<step>step 1|step 2|step 3|last step) of deployment '
                 'process in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_btn_in_deployment_step(selenium, browser_id, btn,
                                       step, onepanel):
    step = getattr(onepanel(selenium[browser_id]).content.deployment,
                   step.replace(' ', ''))
    getattr(step, transform(btn)).click()


@when(parsers.parse('user of {browser_id} waits {timeout:d} seconds '
                    'for cluster deployment to finish'))
@then(parsers.parse('user of {browser_id} waits {timeout:d} seconds '
                    'for cluster deployment to finish'))
def wt_await_finish_of_cluster_deployment(selenium, browser_id,
                                          timeout, modals):
    driver = selenium[browser_id]
    limit = time.time() + timeout
    with implicit_wait(driver, 0.1, SELENIUM_IMPLICIT_WAIT):
        while time.time() < limit:
            try:
                modals(driver).cluster_deployment
            except RuntimeError:
                break
            else:
                time.sleep(1)
                continue
        else:
            raise RuntimeError('cluster deployment exceeded '
                               'time limit: {}'.format(timeout))


@when(parsers.parse('user of {browser_id} selects {storage_type} from storage '
                    'selector in step 3 of deployment process in Onepanel'))
@then(parsers.parse('user of {browser_id} selects {storage_type} from storage '
                    'selector in step 3 of deployment process in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_select_storage_type_in_deployment_step3(selenium, browser_id,
                                               storage_type, onepanel):
    step = onepanel(selenium[browser_id]).content.deployment.step3
    selector = step.form.storage_selector
    if not selector.is_expanded():
        selector.expand()
    selector.storages[storage_type].click()


@when(parsers.re('user of (?P<browser_id>.*?) types "(?P<text>.*?)" to '
                 '(?P<input_box>.*?) field in (?P<form>POSIX) form '
                 'in step 3 of deployment process in Onepanel'))
@then(parsers.re('user of (?P<browser_id>.*?) types "(?P<text>.*?)" to '
                 '(?P<input_box>.*?) field in (?P<form>POSIX) form '
                 'in step 3 of deployment process in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_text_to_in_box_in_deployment_step3(selenium, browser_id, text,
                                               form, onepanel, input_box):
    form = getattr(onepanel(selenium[browser_id]).content.deployment.step3.form,
                   transform(form))
    setattr(form, transform(input_box), text)


@when(parsers.parse('user of {browser_id} clicks on Add button in add storage '
                    'form in step 3 of deployment process in Onepanel'))
@then(parsers.parse('user of {browser_id} clicks on Add button in add storage '
                    'form in step 3 of deployment process in Onepanel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_add_btn_in_storage_add_form(selenium, browser_id, onepanel):
    onepanel(selenium[browser_id]).content.deployment.step3.form.add()
