"""Steps used in clusters deployment process"""

import re
import time

from pytest_bdd import when, then, parsers
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_FRONTEND, SELENIUM_IMPLICIT_WAIT
from tests.gui.utils.generic import repeat_failed, parse_seq, implicit_wait


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} enables {options} options for '
                    '{host_regexp} host in hosts table in step 1 '
                    'of deployment process for "{cluster}" in {panel}'))
@then(parsers.parse('user of {browser_id} enables {options} options for '
                    '{host_regexp} host in hosts table in step 1 '
                    'of deployment process for "{cluster}" in {panel}'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_check_host_options_in_deployment_step1(selenium, browser_id, options,
                                              host_regexp, cluster, panel,
                                              op_panel, oz_panel):
    cls = op_panel if panel == 'op panel' else oz_panel
    panel = cls(select_browser(selenium, browser_id))
    hosts = panel.sidebar.records[cluster].deployment.step1.hosts
    for host in hosts:
        if re.match(host_regexp, host.name):
            for option in parse_seq(options):
                toggle = getattr(host, option.strip().lower().replace(' ', '_'))
                toggle.check()


@when(parsers.parse('user of {browser_id} types ip address of "{host}" zone to '
                    'Onezone domain field in step 2 of deployment process for '
                    '"New cluster" in op panel'))
@then(parsers.parse('user of {browser_id} types ip address of "{host}" zone to '
                    'Onezone domain field in step 2 of deployment process for '
                    '"New cluster" in op panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_zone_ip_to_zone_domain_in_deployment_step2(selenium, browser_id,
                                                       host, hosts, op_panel):
    driver = select_browser(selenium, browser_id)
    step = op_panel(driver).sidebar.records['New cluster'].deployment.step2
    step.oz_domain = hosts['onezone'][host]


@when(parsers.parse('user of {browser_id} types redirection point of "{host}" '
                    'provider to Redirection point field in step 2 of '
                    'deployment process for "New cluster" in op panel'))
@then(parsers.parse('user of {browser_id} types redirection point of "{host}" '
                    'provider to Redirection point field in step 2 of '
                    'deployment process for "New cluster" in op panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_enter_redirection_point_in_deployment_step2(selenium, browser_id,
                                                   host, hosts, op_panel):
    driver = select_browser(selenium, browser_id)
    step = op_panel(driver).sidebar.records['New cluster'].deployment.step2
    step.redirection_point = 'https://{}'.format(hosts['oneprovider'][host])


@when(parsers.re('user of (?P<browser_id>.+?) types "(?P<text>.+?)" to '
                 '(?P<input_box>.+?) field in '
                 '(?P<step>step 1|step 2|step 3|last step) of deployment '
                 'process for "(?P<cluster>.+?)" in (?P<panel>oz|op) panel'))
@then(parsers.re('user of (?P<browser_id>.+?) types "(?P<text>.+?)" to '
                 '(?P<input_box>.+?) field in '
                 '(?P<step>step 1|step 2|step 3|last step) of deployment '
                 'process for "(?P<cluster>.+?)" in (?P<panel>oz|op) panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_text_to_in_box_in_deployment_step(selenium, browser_id, text,
                                              panel, op_panel, oz_panel,
                                              input_box, step, cluster):
    cls = op_panel if panel == 'op' else oz_panel
    panel = cls(select_browser(selenium, browser_id))
    step = getattr(panel.sidebar.records[cluster].deployment,
                   step.replace(' ', ''))
    setattr(step, input_box.strip().lower().replace(' ', '_'), text)


@when(parsers.re('user of (?P<browser_id>.+?) clicks on (?P<btn>.+?) button '
                 'in (?P<step>step 1|step 2|step 3|last step) of deployment '
                 'process for "(?P<cluster>.+?)" in (?P<panel>oz|op) panel'))
@then(parsers.re('user of (?P<browser_id>.+?) clicks on (?P<btn>.+?) button '
                 'in (?P<step>step 1|step 2|step 3|last step) of deployment '
                 'process for "(?P<cluster>.+?)" in (?P<panel>oz|op) panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_btn_in_deployment_step(selenium, browser_id, btn, step,
                                       panel, op_panel, oz_panel, cluster):
    cls = op_panel if panel == 'op panel' else oz_panel
    panel = cls(select_browser(selenium, browser_id))
    step = getattr(panel.sidebar.records[cluster].deployment,
                   step.replace(' ', ''))
    getattr(step, btn.strip().lower().replace(' ', '_')).click()


@when(parsers.parse('user of {browser_id} waits {timeout:d} seconds '
                    'for cluster deployment to finish'))
@then(parsers.parse('user of {browser_id} waits {timeout:d} seconds '
                    'for cluster deployment to finish'))
def wt_await_finish_of_cluster_deployment(selenium, browser_id, timeout, modals):
    driver = select_browser(selenium, browser_id)
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
                    'selector in step 3 of deployment process for "{cluster}" '
                    'in op panel'))
@then(parsers.parse('user of {browser_id} selects {storage_type} from storage '
                    'selector in step 3 of deployment process for "{cluster}" '
                    'in op panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_select_storage_type_in_deployment_step3(selenium, browser_id,
                                               storage_type, op_panel,
                                               cluster):
    driver = select_browser(selenium, browser_id)
    step = op_panel(driver).sidebar.records[cluster].deployment.step3
    selector = step.add_form.storage_selector
    if not selector.is_expanded():
        selector.expand()
    selector.storages[storage_type].click()


@when(parsers.parse('user of {browser_id} types "{text}" to {input_box} '
                    'field in add storage form in step 3 of deployment '
                    'process for "{cluster}" in op panel'))
@then(parsers.parse('user of {browser_id} types "{text}" to {input_box} '
                    'field in add storage form in step 3 of deployment '
                    'process for "{cluster}" in op panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_text_to_in_box_in_deployment_step3(selenium, browser_id, text,
                                               op_panel, input_box, cluster):
    driver = select_browser(selenium, browser_id)
    step = op_panel(driver).sidebar.records[cluster].deployment.step3
    setattr(step.add_form.form, input_box.strip().lower().replace(' ', '_'),
            text)


@when(parsers.parse('user of {browser_id} clicks on Add button in add storage '
                    'form in step 3 of deployment process for "{cluster}" '
                    'in op panel'))
@then(parsers.parse('user of {browser_id} clicks on Add button in add storage '
                    'form in step 3 of deployment process for "{cluster}" '
                    'in op panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_select_storage_type_in_deployment_step3(selenium, browser_id,
                                               op_panel, cluster):
    driver = select_browser(selenium, browser_id)
    op_panel(driver).sidebar.records[cluster].deployment.step3.add_form.add()
