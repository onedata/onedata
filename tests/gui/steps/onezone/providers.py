"""Steps for GO TO YOUR FILES panel and provider popup features of Onezone page.
"""
import itertools
from pytest_bdd import parsers, when, then
from pytest_selenium_multi.pytest_selenium_multi import select_browser

from tests.gui.conftest import WAIT_BACKEND, SELENIUM_IMPLICIT_WAIT
from tests.gui.utils.generic import repeat_failed, implicit_wait

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
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
    def assert_popup_appeared(d, provider_name, msg):
        prov = oz_page(d)['world map'].get_provider_with_displayed_panel()
        assert provider_name == prov.name, msg.format(prov.name,
                                                      provider_name)

    err_msg = 'Popup displayed for provider named "{}" ' \
              'instead of "{}"'
    assert_popup_appeared(driver, provider, err_msg)


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on the '
                 r'"(?P<btn>Go to your files|copy hostname)" button in '
                 r'"(?P<provider>.+?)" provider\'s popup displayed on world map'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on the '
                 r'"(?P<btn>Go to your files|copy hostname)" button in '
                 r'"(?P<provider>.+?)" provider\'s popup displayed on world map'))
def click_on_btn_in_provider_popup(selenium, browser_id, btn, provider, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def click_on_btn(d, provider_name, msg):
        prov = oz_page(d)['world map'].get_provider_with_displayed_panel()
        assert provider_name == prov.name, msg.format(prov.name, provider_name)
        action = getattr(prov, btn.lower().replace(' ', '_'))
        action()

    err_msg = 'Popup displayed for provider named "{}" ' \
              'instead of "{}"'
    click_on_btn(driver, provider, err_msg)


@when(parsers.parse(r'user of {browser_id} unsets provider named "{provider}" '
                    r'from home by clicking on home icon in that provider '
                    r'record in expanded "GO TO YOUR FILES" Onezone panel'))
@then(parsers.parse(r'user of {browser_id} unsets provider named "{provider}" '
                    r'from home by clicking on home icon in that provider '
                    r'record in expanded "GO TO YOUR FILES" Onezone panel'))
def unset_given_item_from_home_by_clicking_on_home_icon(selenium, browser_id,
                                                        provider, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def set_as_home(d, item, msg):
        item_record = oz_page(d)['go to your files'][item]
        item_record.unset_from_home()
        assert not item_record.is_home(), msg.format(item_record.name)

    err_msg = 'provider named "{}" is still set as home but it should not'
    with implicit_wait(driver, 0.2, SELENIUM_IMPLICIT_WAIT):
        set_as_home(driver, provider, err_msg)


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that there is no '
                 r'displayed provider popup next to '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'provider circle on Onezone world map'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that there is no '
                 r'displayed provider popup next to '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'provider circle on Onezone world map'))
@when(parsers.re(r'user of (?P<browser_id>.+?) sees that provider popup next to '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'provider circle on Onezone world map has disappeared'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that provider popup next to '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'provider circle on Onezone world map has disappeared'))
def assert_no_provider_popup_next_to_provider_circle(selenium, browser_id,
                                                     ordinal, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND)
    def assert_not_displayed(d, index, msg):
        world_map = oz_page(d)['world map']
        provider = world_map[index]
        assert not provider.is_displayed(), msg.format(ordinal)

    err_msg = 'provider popup for {} circle is displayed ' \
              'while it should not be'
    with implicit_wait(driver, 0.2, SELENIUM_IMPLICIT_WAIT):
        assert_not_displayed(driver, int(ordinal[:-2]) - 1, err_msg)


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'provider circle on Onezone world map'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'provider circle on Onezone world map'))
def click_on_provider_circle(selenium, browser_id, ordinal, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def click_on_provider(d, index):
        world_map = oz_page(d)['world map']
        provider = world_map[index]
        provider.click()

    click_on_provider(driver, int(ordinal[:-2]) - 1)


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that provider popup has '
                 r'appeared next to (?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|'
                 r'\d*?11th|\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|'
                 r'\d*?[^1]3rd) provider circle on Onezone world map'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that provider popup has '
                 r'appeared next to (?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|'
                 r'\d*?11th|\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|'
                 r'\d*?[^1]3rd) provider circle on Onezone world map'))
def assert_provider_popup_next_to_provider_circle(selenium, browser_id,
                                                  ordinal, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_provider_popup(d, index, msg):
        world_map = oz_page(d)['world map']
        provider = world_map[index]
        assert provider.is_displayed(), msg.format(ordinal)

    err_msg = 'provider popup for {} circle is not displayed ' \
              'while it should be'
    with implicit_wait(driver, 0.2, SELENIUM_IMPLICIT_WAIT):
        assert_provider_popup(driver, int(ordinal[:-2]) - 1, err_msg)


@when(parsers.parse('user of {browser_id} clicks on Onezone world map'))
@then(parsers.parse('user of {browser_id} clicks on Onezone world map'))
def click_on_world_map(selenium, browser_id, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def click_on_map(d):
        world_map = oz_page(d)['world map']
        world_map.click()

    click_on_map(driver)


@when(parsers.parse('user of {browser_id} sees that the list of spaces '
                    'in provider popup and in expanded "GO TO YOUR FILES" '
                    'Onezone panel are the same for provider named "{provider}"'))
@then(parsers.parse('user of {browser_id} sees that the list of spaces '
                    'in provider popup and in expanded "GO TO YOUR FILES" '
                    'Onezone panel are the same for provider named "{provider}"'))
def assert_consistent_list_of_spaces_for_provider(selenium, browser_id,
                                                  provider, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_consistency(d, provider_name, msg1, msg2):
        prov_record = oz_page(d)['go to your files'][provider_name]
        prov_popup = oz_page(d)['world map'].get_provider_with_displayed_panel()
        assert provider_name == prov_popup.name, msg1.format(prov_popup.name,
                                                             provider_name)

        for space1, space2 in itertools.izip(prov_popup, prov_record):
            name1, is_home1 = space1.name, space1.is_home
            name2, is_home2 = space2.name, space2.is_home
            assert (name1, is_home1) == (name2, is_home2), \
                msg2.format(name1=name1, is_home1=is_home1,
                            name2=name2, is_home2=is_home2)

    err_msg1 = 'Popup displayed for provider named "{}" ' \
               'instead of "{}"'
    err_msg2 = 'mismatch between provider popup ({name1}, {is_home1}) ' \
               'and provider record ({name2}, {is_home2}) ' \
               'in GO TO YOUR FILES panel in space list found'
    with implicit_wait(driver, 0.1, SELENIUM_IMPLICIT_WAIT):
        assert_consistency(driver, provider, err_msg1, err_msg2)


@when(parsers.parse('user of {browser_id} clicks on "{provider}" provider '
                    'in expanded "GO TO YOUR FILES" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "{provider}" provider '
                    'in expanded "GO TO YOUR FILES" Onezone panel'))
def click_on_provider_in_go_to_your_files_oz_panel(selenium, browser_id,
                                                   provider, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def click_on_provider(d, provider_name):
        prov_record = oz_page(d)['go to your files'][provider_name]
        prov_record.click()

    click_on_provider(driver, provider)


@when(parsers.parse('user of {browser_id} sees that provider named "{provider}" '
                    'in expanded "GO TO YOUR FILES" Onezone panel is working'))
@then(parsers.parse('user of {browser_id} sees that provider named "{provider}" '
                    'in expanded "GO TO YOUR FILES" Onezone panel is working'))
def assert_provider_working_in_oz_panel(selenium, browser_id,
                                        provider, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_working(d, provider_name, msg):
        prov_record = oz_page(d)['go to your files'][provider_name]
        assert prov_record.is_working(), msg.format(prov_record.name)

    err_msg = 'provider icon in GO TO YOUR FILES oz panel for ' \
              '"{}" is not green'
    assert_working(driver, provider, err_msg)


@when(parsers.parse('user of {browser_id} sees that provider named "{provider}" '
                    'in expanded "GO TO YOUR FILES" Onezone panel is not working'))
@then(parsers.parse('user of {browser_id} sees that provider named "{provider}" '
                    'in expanded "GO TO YOUR FILES" Onezone panel is not working'))
def assert_provider_not_working_in_oz_panel(selenium, browser_id,
                                            provider, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_not_working(d, provider_name, msg):
        prov_record = oz_page(d)['go to your files'][provider_name]
        assert prov_record.is_not_working(), msg.format(prov_record.name)

    err_msg = 'provider icon in GO TO YOUR FILES oz panel for ' \
              '"{}" is not gray'
    assert_not_working(driver, provider, err_msg)


@when(parsers.parse('user of {browser_id} sees alert with title "{title}" '
                    'on world map in Onezone gui'))
@then(parsers.parse('user of {browser_id} sees alert with title "{title}" '
                    'on world map in Onezone gui'))
def assert_alert_with_title_in_oz(selenium, browser_id, title, oz_page):
    driver = select_browser(selenium, browser_id)

    @repeat_failed(attempts=WAIT_BACKEND, timeout=True)
    def assert_alert(d, alert_title, msg):
        alert = oz_page(d)['world map'].message
        assert alert.title.lower() == alert_title.lower(), msg.format(alert.title,
                                                                      alert_title)

    err_msg = 'alert title {} does not match {}'
    assert_alert(driver, title, err_msg)
