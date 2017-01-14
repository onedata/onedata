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


@when(parsers.re('user of (?P<browser_id>.+?) clicks on the '
                 '"(?P<btn>Go to your files|copy hostname)" button in '
                 '"(?P<provider>.+?)" provider\'s popup displayed on world map'))
@then(parsers.re('user of (?P<browser_id>.+?) clicks on the '
                 '"(?P<btn>Go to your files|copy hostname)" button in '
                 '"(?P<provider>.+?)" provider\'s popup displayed on world map'))
def click_on_btn_in_provider_popup(selenium, browser_id, btn, provider, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def click_on_btn(d, provider_name):
        prov = oz_page(d)['world map'].get_provider_with_displayed_panel()
        err_msg = 'Popup displayed for provider named "{}" ' \
                  'instead of "{}"'.format(prov.name, provider_name)
        assert provider_name == prov.name, err_msg
        action = getattr(prov, btn.lower().replace(' ', '_'))
        action()

    click_on_btn(driver, provider)
