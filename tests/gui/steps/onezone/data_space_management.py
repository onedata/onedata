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
def expand_settings_dropdown_for_space_in_panel(selenium, browser_id,
                                                name, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def expand_settings_dropdown(d, space_name):
        oz_page(d)['data space management'][space_name].settings.expand()

    expand_settings_dropdown(driver, name)


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on the '
                 r'"(?P<option>LEAVE|RENAME|GET SUPPORT|SET AS HOME)" item in '
                 r'settings dropdown for space named "(?P<name>.+?)" '
                 r'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on the '
                 r'"(?P<option>LEAVE|RENAME|GET SUPPORT|SET AS HOME)" item in '
                 r'settings dropdown for space named "(?P<name>.+?)" '
                 r'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
def click_on_settings_option_for_space_in_panel(selenium, browser_id,
                                                option, name, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def click_on_settings_item(d, space_name, chosen_option):
        settings = oz_page(d)['data space management'][space_name].settings
        action = getattr(settings, chosen_option)
        action()

    click_on_settings_item(driver, name, option.lower().replace(' ', '_'))


@when(parsers.parse('user of {browser_id} clicks on "Get support" button in '
                    'submenu for "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "Get support" button in '
                    'submenu for "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
def click_on_get_support_btn_for_space_in_oz_panel(selenium, browser_id,
                                                   name, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def expand_submenu_for_space(d, space_name):
        oz_page(d)['data space management'][space_name].get_support()

    expand_submenu_for_space(driver, name)


@when(parsers.parse('user of {browser_id} sees that dropright with token for '
                    'space named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel has appeared'))
@then(parsers.parse('user of {browser_id} sees that dropright with token for '
                    'space named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel has appeared'))
def assert_dropright_witk_token_for_space_appeared(selenium, browser_id,
                                                   name, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_dropright_appeared(d, space_name):
        # noinspection PyStatementEffect
        oz_page(d)['data space management'][space_name].dropright_with_token

    assert_dropright_appeared(driver, name)


@when(parsers.parse('user of {browser_id} sees that dropright contains '
                    'non-empty token for space named "{name}" in expanded '
                    '"DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees that dropright contains '
                    'non-empty token for space named "{name}" in expanded '
                    '"DATA SPACE MANAGEMENT" Onezone panel'))
def assert_dropright_has_nonempty_token_for_space_appeared(selenium, browser_id,
                                                           name, tmp_memory,
                                                           oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_nonempty_token(d, space_name):
        dropright = oz_page(d)['data space '
                               'management'][space_name].dropright_with_token
        token = dropright.token
        assert token, 'no dropright with nonempty token for space named "{}" ' \
                      'found'.format(space_name)
        return token

    tmp_memory[browser_id]['token'] = assert_nonempty_token(driver, name)


@when(parsers.parse('user of {browser_id} copy token from dropright for '
                    'space named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@then(parsers.parse('user of {browser_id} copy token from dropright for '
                    'space named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
def copy_token_from_droprigth_for_space(selenium, browser_id, name, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def copy_token(d, space_name):
        dropright = oz_page(d)['data space '
                               'management'][space_name].dropright_with_token
        dropright.copy_token()

    copy_token(driver, name)


@when(parsers.parse('user of {browser_id} clicks on "{provider}" provider in submenu '
                    'of space named "{space}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "{provider}" provider in submenu '
                    'of space named "{space}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
def click_on_supporting_provider_for_space_in_space_submenu(selenium, browser_id,
                                                            provider, space,
                                                            oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def click_on_provider(d, space_name, provider_name):
        prov = oz_page(d)['data space management'][space_name][provider_name]
        prov.click()

    click_on_provider(driver, space, provider)


@when(parsers.parse('user of {browser_id} clicks on unsupport space for provider '
                    'named "{provider}" in submenu of space named "{space}" '
                    'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on unsupport space for provider '
                    'named "{provider}" in submenu of space named "{space}" '
                    'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
def click_on_unsupport_space_for_supporting_provider(selenium, browser_id,
                                                     provider, space, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def unsupport_space(d, space_name, provider_name):
        prov = oz_page(d)['data space management'][space_name][provider_name]
        prov.unsupport_space()

    unsupport_space(driver, space, provider)


@when(parsers.parse('user of {browser_id} sees that list of supporting '
                    'providers for space named "{name}" in expanded "DATA '
                    'SPACE MANAGEMENT" Onezone panel contains only: '
                    '{providers_list}'))
@then(parsers.parse('user of {browser_id} sees that list of supporting '
                    'providers for space named "{name}" in expanded "DATA '
                    'SPACE MANAGEMENT" Onezone panel contains only: '
                    '{providers_list}'))
def assert_supporting_providers_for_space_in_panel(selenium, browser_id, name,
                                                   providers_list, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_supporting_providers_for_space(d, space_name,
                                              expected_providers_list,
                                              msg1, msg2):
        space_record = oz_page(d)['data space management'][space_name]
        actual_provs = space_record.supporting_providers
        actual_provs_num = len(actual_provs)
        expected_provs = set(expected_providers_list)
        expected_provs_num = len(expected_provs)

        assert expected_provs_num == actual_provs_num, msg1.format(expected_provs_num,
                                                                   actual_provs_num)
        for provider in expected_provs:
            assert provider in actual_provs, msg2.format(provider)

    err_msg1 = 'Expected number of providers {} does not match actual {}'
    err_msg2 = 'space named "{name}" does not have supporting provider ' \
               'named "{{}}" as it should'.format(name=name)
    assert_supporting_providers_for_space(driver, name,
                                          parse_seq(providers_list),
                                          err_msg1, err_msg2)


@when(parsers.parse('user of {browser_id} sees that there is/are no supporting '
                    'provider(s) named {providers_list} for space named '
                    '"{space}" in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees that there is/are no supporting '
                    'provider(s) named {providers_list} for space named '
                    '"{space}" in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
def assert_no_such_supporting_providers_for_space(selenium, browser_id, space,
                                                  providers_list, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_no_such_providers(d, space_name, not_expected_providers, msg):
        space_record = oz_page(d)['data space management'][space_name]
        supporting_providers = space_record.supporting_providers

        for provider in not_expected_providers:
            assert provider not in supporting_providers, msg.format(provider)

    err_msg = 'space named "{}" has supporting provider named "{{}}" ' \
              'while it should not have'.format(space)
    with implicit_wait(driver, 0.5, SELENIUM_IMPLICIT_WAIT):
        assert_no_such_providers(driver, space,
                                 parse_seq(providers_list), err_msg)
