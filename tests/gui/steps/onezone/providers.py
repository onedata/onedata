"""Steps for GO TO YOUR FILES panel and provider popup features of Onezone page.
"""

from pytest_bdd import parsers, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_BACKEND
from tests.gui.utils.generic import repeat_failed

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} sees that provider popup for provider '
                    'named "{provider}" has appeared on world map'))
@then(parsers.parse('user of {browser_id} sees that provider popup for provider '
                    'named "{provider}" has appeared on world map'))
def assert_provider_popup_has_appeared_on_map(selenium, browser_id,
                                              provider, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_popup_appeared(d, provider_name):
        prov = oz_page(d)['world map'].get_provider_with_displayed_panel()
        err_msg = 'Popup displayed for provider named "{}" ' \
                  'instead of "{}"'.format(prov.name, provider_name)
        assert provider_name == prov.name, err_msg

    assert_popup_appeared(driver, provider)


@when(parsers.parse('user of browser clicks on the "Go to your files" button '
                    'in "{provider}" provider\'s popup displayed on world map'))
@then(parsers.parse('user of browser clicks on the "Go to your files" button '
                    'in "{provider}" provider\'s popup displayed on world map'))
def click_on_go_to_your_files_in_provider_popup(selenium, browser_id, provider,
                                                oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def click_on_btn(d, provider_name):
        prov = oz_page(d)['world map'].get_provider_with_displayed_panel()
        err_msg = 'Popup displayed for provider named "{}" ' \
                  'instead of "{}"'.format(prov.name, provider_name)
        assert provider_name == prov.name, err_msg
        prov.go_to_your_files()

    click_on_btn(driver, provider)


@when(parsers.parse('user of {browser_id} sees that spaces counter for '
                    '"{provider}" match number of displayed supported spaces '
                    'in expanded submenu of given provider in expanded '
                    '"GO TO YOUR FILES" Onezone panel'))
@then(parsers.parse('user of {browser_id} sees that spaces counter for '
                    '"{provider}" match number of displayed supported spaces '
                    'in expanded submenu of given provider in expanded '
                    '"GO TO YOUR FILES" Onezone panel'))
def assert_number_of_spaces_match_spaces_counter(selenium, browser_id,
                                                 provider, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_match(d, provider_name):
        provider_record = oz_page(d)['go to your files'][provider_name]
        supported_spaces = len(provider_record.supported_spaces)
        spaces_counter = provider_record.spaces_count

        err_msg = 'Spaces counter number {} does not match displayed number ' \
                  'of supported spacess {}'.format(spaces_counter,
                                                   supported_spaces)
        assert spaces_counter == supported_spaces, err_msg

    assert_match(driver, provider)


@when(parsers.parse('user of {browser_id} expands submenu of "{name}" by '
                    'clicking on cloud in provider record in expanded '
                    '"GO TO YOUR FILES" Onezone panel'))
@then(parsers.parse('user of {browser_id} expands submenu of "{name}" by '
                    'clicking on cloud in provider record in expanded '
                    '"GO TO YOUR FILES" Onezone panel'))
def expand_provider_submenu_in_oz_panel(selenium, browser_id, name, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def expand_submenu_for_space(d, provider_name):
        provider_record = oz_page(d)['go to your files'][provider_name]
        err_msg = 'submenu for provider named "{name}" has not been ' \
                  'expanded'.format(name=provider_name)
        provider_record.expand_submenu()
        assert provider_record.is_submenu_expanded is True, err_msg

    expand_submenu_for_space(driver, name)
