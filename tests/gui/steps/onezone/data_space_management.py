"""Steps for DATA SPACE MANAGEMENT panel features of Onezone page.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from pytest_bdd import parsers, when, then

from tests.gui.conftest import WAIT_BACKEND, WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, parse_seq


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'(?P<btn>confirm|cancel) button displayed next to space '
                 r'creation edit box in expanded "DATA SPACE MANAGEMENT" '
                 r'Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'(?P<btn>confirm|cancel) button displayed next to space '
                 r'creation edit box in expanded "DATA SPACE MANAGEMENT" '
                 r'Onezone panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_btn_for_space_creation_edit_box_in_oz(selenium, browser_id,
                                                   btn, oz_page):
    spaces_panel = oz_page(selenium[browser_id])['data space management']
    getattr(spaces_panel.create_space_edit_box, btn).click()


@when(parsers.parse('user of {browser_id} types "{text}" to space creation '
                    'edit box in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@then(parsers.parse('user of {browser_id} types "{text}" to space creation '
                    'edit box in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def type_text_into_space_creation_edit_box_in_oz(selenium, browser_id,
                                                 text, oz_page):
    spaces_panel = oz_page(selenium[browser_id])['data space management']
    spaces_panel.create_space_edit_box.value = text


@when(parsers.parse('user of {browser_id} expands settings dropdown for space '
                    'named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel by clicking on settings icon'))
@then(parsers.parse('user of {browser_id} expands settings dropdown for space '
                    'named "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel by clicking on settings icon'))
@repeat_failed(timeout=WAIT_FRONTEND)
def expand_settings_dropdown_for_space_in_oz(selenium, browser_id,
                                             name, oz_page):
    driver = selenium[browser_id]
    oz_page(driver)['data space management'].spaces[name].settings.expand()


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on the '
                 r'"(?P<option>LEAVE|RENAME|ADD STORAGE|SET AS HOME)" item in '
                 r'settings dropdown for space named "(?P<name>.+?)" '
                 r'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on the '
                 r'"(?P<option>LEAVE|RENAME|ADD STORAGE|SET AS HOME)" item in '
                 r'settings dropdown for space named "(?P<name>.+?)" '
                 r'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_settings_option_for_space_in_oz(selenium, browser_id,
                                             option, name, oz_page):
    driver = selenium[browser_id]
    settings = oz_page(driver)['data space management'].spaces[name].settings
    getattr(settings, option.lower().replace(' ', '_'))()


@when(parsers.parse('user of {browser_id} clicks on "Add storage" button in '
                    'submenu for "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "Add storage" button in '
                    'submenu for "{name}" in expanded "DATA SPACE MANAGEMENT" '
                    'Onezone panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_get_support_btn_for_space_in_oz_panel(selenium, browser_id,
                                                   name, oz_page):
    driver = selenium[browser_id]
    oz_page(driver)['data space management'].spaces[name].add_storage()


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
    driver = selenium[browser_id]
    space_item = oz_page(driver)['data space management'].spaces[space]
    space_item.providers[provider].click()


@when(parsers.parse('user of {browser_id} clicks on unsupport space for provider '
                    'named "{provider}" in submenu of space named "{space}" '
                    'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on unsupport space for provider '
                    'named "{provider}" in submenu of space named "{space}" '
                    'in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def click_on_unsupport_space_for_supporting_provider(selenium, browser_id,
                                                     provider, space, oz_page):
    driver = selenium[browser_id]
    space_item = oz_page(driver)['data space management'].spaces[space]
    space_item.providers[provider].unsupport_space()


@when(parsers.parse('user of {browser_id} sees that list of supporting '
                    'providers for space named "{name}" in expanded "DATA '
                    'SPACE MANAGEMENT" Onezone panel contains only: '
                    '{providers_list}'))
@then(parsers.parse('user of {browser_id} sees that list of supporting '
                    'providers for space named "{name}" in expanded "DATA '
                    'SPACE MANAGEMENT" Onezone panel contains only: '
                    '{providers_list}'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_supporting_providers_for_space_in_oz(selenium, browser_id, name,
                                                providers_list, oz_page):
    driver = selenium[browser_id]
    space_item = oz_page(driver)['data space management'].spaces[name]
    displayed_providers = {provider.name for provider in space_item.providers}
    expected_providers = set(parse_seq(providers_list))
    assert displayed_providers == expected_providers, \
        ('expected only {} as supporting providers, '
         'but displayed are {}'.format(expected_providers, displayed_providers))


@when(parsers.parse('user of {browser_id} sees that there is/are no supporting '
                    'provider(s) named {providers_list} for space named '
                    '"{space}" in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees that there is/are no supporting '
                    'provider(s) named {providers_list} for space named '
                    '"{space}" in expanded "DATA SPACE MANAGEMENT" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_no_such_supporting_providers_for_space(selenium, browser_id, space,
                                                  providers_list, oz_page):
    driver = selenium[browser_id]
    space_item = oz_page(driver)['data space management'].spaces[space]
    displayed_providers = {provider.name for provider in space_item.providers}
    err_msg = 'space named "{}" has supporting provider named "{{}}" ' \
              'while it should not have'.format(space)
    for provider in parse_seq(providers_list):
        assert provider not in displayed_providers, err_msg.format(provider)
