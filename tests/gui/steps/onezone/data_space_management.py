"""Steps for DATA SPACE MANAGEMENT panel features of Onezone page.
"""

from pytest_bdd import parsers, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_BACKEND, SELENIUM_IMPLICIT_WAIT
from tests.gui.utils.generic import repeat_failed, parse_seq, implicit_wait

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} focuses on activated edit box for '
                    'creating new space in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@then(parsers.parse('user of {browser_id} focuses on activated edit box for '
                    'creating new space in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
def get_create_space_edit_box(selenium, browser_id, tmp_memory, oz_page):
    driver = select_browser(selenium, browser_id)
    edit_box = oz_page(driver)['data space management'].create_space_edit_box
    tmp_memory[browser_id]['edit_box'] = edit_box


@when(parsers.parse('user of {browser_id} expands settings dropdown for space '
                    'named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel by clicking on settings icon'))
@then(parsers.parse('user of {browser_id} expands settings dropdown for space '
                    'named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel by clicking on settings icon'))
@repeat_failed(timeout=WAIT_BACKEND)
def expand_settings_dropdown_for_space_in_panel(selenium, browser_id,
                                                name, oz_page):
    driver = select_browser(selenium, browser_id)
    oz_page(driver)['data space management'].spaces[name].settings.expand()


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on the '
                 r'"(?P<option>LEAVE|RENAME|GET SUPPORT|SET AS HOME)" item in '
                 r'settings dropdown for space named "(?P<name>.+?)" '
                 r'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on the '
                 r'"(?P<option>LEAVE|RENAME|GET SUPPORT|SET AS HOME)" item in '
                 r'settings dropdown for space named "(?P<name>.+?)" '
                 r'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def click_on_settings_option_for_space_in_panel(selenium, browser_id,
                                                option, name, oz_page):
    driver = select_browser(selenium, browser_id)
    settings = oz_page(driver)['data space management'].spaces[name].settings
    action = getattr(settings, option.lower().replace(' ', '_'))
    action()


@when(parsers.parse('user of {browser_id} clicks on "Get support" button in '
                    'submenu for "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "Get support" button in '
                    'submenu for "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def click_on_get_support_btn_for_space_in_oz_panel(selenium, browser_id,
                                                   name, oz_page):
    driver = select_browser(selenium, browser_id)
    oz_page(driver)['data space management'].spaces[name].get_support()


@when(parsers.parse('user of {browser_id} sees that dropright with token for '
                    'space named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel has appeared'))
@then(parsers.parse('user of {browser_id} sees that dropright with token for '
                    'space named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel has appeared'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_dropright_witk_token_for_space_appeared(selenium, browser_id,
                                                   name, oz_page):
    driver = select_browser(selenium, browser_id)
    # noinspection PyStatementEffect
    oz_page(driver)['data space management'].spaces[name].dropright_with_token


@when(parsers.parse('user of {browser_id} sees that dropright contains '
                    'non-empty token for space named "{name}" in expanded '
                    '"DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees that dropright contains '
                    'non-empty token for space named "{name}" in expanded '
                    '"DATA SPACE MANAGEMENT" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_dropright_has_nonempty_token_for_space_appeared(selenium, browser_id,
                                                           name, tmp_memory,
                                                           oz_page):
    driver = select_browser(selenium, browser_id)
    space_rec = oz_page(driver)['data space management'].spaces[name]
    token = space_rec.dropright_with_token.token
    assert token, 'no dropright with nonempty token for space named "{}" ' \
                  'found'.format(name)
    tmp_memory[browser_id]['token'] = token


@when(parsers.parse('user of {browser_id} copy token from dropright for '
                    'space named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@then(parsers.parse('user of {browser_id} copy token from dropright for '
                    'space named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def copy_token_from_droprigth_for_space(selenium, browser_id, name, oz_page):
    driver = select_browser(selenium, browser_id)
    item = oz_page(driver)['data space management'].spaces[name]
    item.dropright_with_token.copy_token()


@when(parsers.parse('user of {browser_id} clicks on "{provider}" provider '
                    'in submenu of space named "{space}" in expanded '
                    '"DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "{provider}" provider '
                    'in submenu of space named "{space}" in expanded '
                    '"DATA SPACE MANAGEMENT" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def click_on_supporting_provider_for_space_in_space_submenu(selenium, browser_id,
                                                            provider, space,
                                                            oz_page):
    driver = select_browser(selenium, browser_id)
    space_rec = oz_page(driver)['data space management'].spaces[space]
    space_rec.providers[provider].click()


@when(parsers.parse('user of {browser_id} clicks on unsupport space for provider '
                    'named "{provider}" in submenu of space named "{space}" '
                    'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on unsupport space for provider '
                    'named "{provider}" in submenu of space named "{space}" '
                    'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def click_on_unsupport_space_for_supporting_provider(selenium, browser_id,
                                                     provider, space, oz_page):
    driver = select_browser(selenium, browser_id)
    space_rec = oz_page(driver)['data space management'].spaces[space]
    space_rec.providers[provider].unsupport_space()


@when(parsers.parse('user of {browser_id} sees that list of supporting '
                    'providers for space named "{name}" in expanded "DATA '
                    'SPACE MANAGEMENT" Onezone panel contains only: '
                    '{providers_list}'))
@then(parsers.parse('user of {browser_id} sees that list of supporting '
                    'providers for space named "{name}" in expanded "DATA '
                    'SPACE MANAGEMENT" Onezone panel contains only: '
                    '{providers_list}'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_supporting_providers_for_space_in_panel(selenium, browser_id, name,
                                                   providers_list, oz_page):
    driver = select_browser(selenium, browser_id)
    space_rec = oz_page(driver)['data space management'].spaces[name]
    providers = space_rec.providers
    expected_providers = set(parse_seq(providers_list))
    assert providers.count() == len(expected_providers), \
        'Expected number of providers {} does not match ' \
        'actual {}'.format(len(expected_providers), providers.count())

    err_msg = 'space named "{name}" does not have supporting provider ' \
              'named "{{}}" while it should'.format(name=name)
    for provider in expected_providers:
        assert provider in providers, err_msg.format(provider)


@when(parsers.parse('user of {browser_id} sees that there is/are no supporting '
                    'provider(s) named {providers_list} for space named '
                    '"{space}" in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees that there is/are no supporting '
                    'provider(s) named {providers_list} for space named '
                    '"{space}" in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_no_such_supporting_providers_for_space(selenium, browser_id, space,
                                                  providers_list, oz_page):
    driver = select_browser(selenium, browser_id)
    space_rec = oz_page(driver)['data space management'].spaces[space]
    providers = space_rec.providers
    err_msg = 'space named "{}" has supporting provider named "{{}}" ' \
              'while it should not have'.format(space)
    with implicit_wait(driver, 0.5, SELENIUM_IMPLICIT_WAIT):
        for provider in parse_seq(providers_list):
            assert provider not in providers, err_msg.format(provider)
