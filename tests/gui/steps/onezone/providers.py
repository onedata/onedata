"""Steps for GO TO YOUR FILES panel and provider popup features of Onezone page.
"""

import time
import itertools

from pytest_bdd import given
from pytest_bdd import parsers, when, then

from tests.gui.conftest import WAIT_BACKEND, SELENIUM_IMPLICIT_WAIT, WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, implicit_wait, redirect_display

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.parse('user of {browser_id} sees that provider popup for provider '
                    'named "{provider}" has appeared on world map'))
@then(parsers.parse('user of {browser_id} sees that provider popup for provider '
                    'named "{provider}" has appeared on world map'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_provider_popup_has_appeared_on_map(selenium, browser_id,
                                              provider, oz_page):
    driver = selenium[browser_id]
    err_msg = 'Popup displayed for provider named "{}" ' \
              'instead of "{}"'
    prov = oz_page(driver)['world map'].get_provider_with_displayed_popup()
    assert provider == prov.name, err_msg.format(prov.name, provider)


@when(parsers.parse('user of {browser_id} sees that hostname in displayed '
                    'provider popup matches that of "{host}" provider'))
@then(parsers.parse('user of {browser_id} sees that hostname in displayed '
                    'provider popup matches that of "{host}" provider'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_provider_hostname_matches_known_ip(selenium, browser_id,
                                              host, oz_page, hosts):
    driver = selenium[browser_id]
    prov = oz_page(driver)['world map'].get_provider_with_displayed_popup()
    ip = hosts['oneprovider'][host]
    displayed_ip = prov.hostname
    assert displayed_ip == ip, \
        'displayed {} provider hostname instead ' \
        'of expected {}'.format(displayed_ip, ip)


@when(parsers.parse('user of {browser_id} sees that hostname in displayed '
                    'provider popup matches {ip} ip address'))
@then(parsers.parse('user of {browser_id} sees that hostname in displayed '
                    'provider popup matches {ip} ip address'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_provider_hostname_matches_given_ip(selenium, browser_id, ip, oz_page):
    driver = selenium[browser_id]
    prov = oz_page(driver)['world map'].get_provider_with_displayed_popup()
    displayed_ip = prov.hostname
    assert displayed_ip == ip, \
        'displayed {} provider hostname instead ' \
        'of expected {}'.format(displayed_ip, ip)


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on the '
                 r'"(?P<btn>Go to your files|copy hostname)" button in '
                 r'"(?P<provider>.+?)" provider\'s popup displayed on world map'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on the '
                 r'"(?P<btn>Go to your files|copy hostname)" button in '
                 r'"(?P<provider>.+?)" provider\'s popup displayed on world map'))
@repeat_failed(timeout=WAIT_BACKEND)
def click_on_btn_in_provider_popup(selenium, browser_id, btn, provider, oz_page):
    driver = selenium[browser_id]
    err_msg = 'Popup displayed for provider named "{}" ' \
              'instead of "{}"'
    prov = oz_page(driver)['world map'].get_provider_with_displayed_popup()
    assert provider == prov.name, err_msg.format(prov.name, provider)
    action = getattr(prov, btn.lower().replace(' ', '_'))
    action()


@when(parsers.parse(r'user of {browser_id} unsets provider named "{provider}" '
                    r'from home by clicking on home icon in that provider '
                    r'record in expanded "GO TO YOUR FILES" Onezone panel'))
@then(parsers.parse(r'user of {browser_id} unsets provider named "{provider}" '
                    r'from home by clicking on home icon in that provider '
                    r'record in expanded "GO TO YOUR FILES" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def unset_given_item_from_home_by_clicking_on_home_icon(selenium, browser_id,
                                                        provider, oz_page):
    driver = selenium[browser_id]
    item = oz_page(driver)['go to your files'].providers[provider]
    err_msg = 'provider named "{}" is still set as home but it should not'
    with implicit_wait(driver, 0.2, SELENIUM_IMPLICIT_WAIT):
        item.unset_from_home()
        assert not item.is_home(), err_msg.format(provider)


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
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_no_provider_popup_next_to_provider_circle(selenium, browser_id,
                                                     ordinal, oz_page):
    driver = selenium[browser_id]
    item = oz_page(driver)['world map'].providers[int(ordinal[:-2]) - 1]
    err_msg = 'provider popup for {} circle is displayed ' \
              'while it should not be'
    with implicit_wait(driver, 0.2, SELENIUM_IMPLICIT_WAIT):
        assert not item.is_displayed(), err_msg.format(ordinal)


@when(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'provider circle on Onezone world map'))
@then(parsers.re(r'user of (?P<browser_id>.+?) clicks on '
                 r'(?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|\d*?11th|'
                 r'\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|\d*?[^1]3rd) '
                 r'provider circle on Onezone world map'))
@repeat_failed(timeout=WAIT_FRONTEND)
def click_on_provider_circle(selenium, browser_id, ordinal, oz_page):
    driver = selenium[browser_id]
    item = oz_page(driver)['world map'].providers[int(ordinal[:-2]) - 1]
    item.click()


@when(parsers.re(r'user of (?P<browser_id>.+?) sees that provider popup has '
                 r'appeared next to (?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|'
                 r'\d*?11th|\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|'
                 r'\d*?[^1]3rd) provider circle on Onezone world map'))
@then(parsers.re(r'user of (?P<browser_id>.+?) sees that provider popup has '
                 r'appeared next to (?P<ordinal>1st|2nd|3rd|\d*?[4567890]th|'
                 r'\d*?11th|\d*?12th|\d*?13th|\d*?[^1]1st|\d*?[^1]2nd|'
                 r'\d*?[^1]3rd) provider circle on Onezone world map'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_provider_popup_next_to_provider_circle(selenium, browser_id,
                                                  ordinal, oz_page):
    driver = selenium[browser_id]
    item = oz_page(driver)['world map'].providers[int(ordinal[:-2]) - 1]
    err_msg = 'provider popup for {} circle is not displayed ' \
              'while it should be'
    with implicit_wait(driver, 0.2, SELENIUM_IMPLICIT_WAIT):
        assert item.is_displayed(), err_msg.format(ordinal)


@when(parsers.parse('user of {browser_id} clicks on Onezone world map'))
@then(parsers.parse('user of {browser_id} clicks on Onezone world map'))
@repeat_failed(timeout=WAIT_BACKEND)
def click_on_world_map(selenium, browser_id, oz_page):
    driver = selenium[browser_id]
    oz_page(driver)['world map'].click()


@when(parsers.parse('user of {browser_id} sees that the list of spaces '
                    'in provider popup and in expanded "GO TO YOUR FILES" '
                    'Onezone panel are the same for provider named "{provider}"'))
@then(parsers.parse('user of {browser_id} sees that the list of spaces '
                    'in provider popup and in expanded "GO TO YOUR FILES" '
                    'Onezone panel are the same for provider named "{provider}"'))
@repeat_failed(timeout=WAIT_FRONTEND)
def assert_consistent_list_of_spaces_for_provider(selenium, browser_id,
                                                  provider, oz_page):
    driver = selenium[browser_id]
    prov_record = oz_page(driver)['go to your files'].providers[provider]
    prov_popup = oz_page(driver)['world map'].get_provider_with_displayed_popup()
    err_msg1 = 'Popup displayed for provider named "{}" ' \
               'instead of "{}"'
    err_msg2 = 'mismatch between provider popup ({name1}, {is_home1}) ' \
               'and provider record ({name2}, {is_home2}) ' \
               'in GO TO YOUR FILES panel in space list found'
    with implicit_wait(driver, 0.1, SELENIUM_IMPLICIT_WAIT):
        assert provider == prov_popup.name, err_msg1.format(prov_popup.name,
                                                            provider)

        for space1, space2 in itertools.izip(prov_popup.spaces,
                                             prov_record.spaces):
            name1, is_home1 = space1.name, space1.is_home()
            name2, is_home2 = space2.name, space2.is_home()
            assert (name1, is_home1) == (name2, is_home2), \
                err_msg2.format(name1=name1, is_home1=is_home1,
                                name2=name2, is_home2=is_home2)


@when(parsers.parse('user of {browser_id} clicks on "{provider}" provider '
                    'in expanded "GO TO YOUR FILES" Onezone panel'))
@then(parsers.parse('user of {browser_id} clicks on "{provider}" provider '
                    'in expanded "GO TO YOUR FILES" Onezone panel'))
@repeat_failed(timeout=WAIT_BACKEND)
def click_on_provider_in_go_to_your_files_oz_panel(selenium, browser_id,
                                                   provider, oz_page):
    driver = selenium[browser_id]
    prov_rec = oz_page(driver)['go to your files'].providers[provider]
    prov_rec.click()


@when(parsers.parse('user of {browser_id} sees that provider named "{provider}" '
                    'in expanded "GO TO YOUR FILES" Onezone panel is working'))
@then(parsers.parse('user of {browser_id} sees that provider named "{provider}" '
                    'in expanded "GO TO YOUR FILES" Onezone panel is working'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_provider_working_in_oz_panel(selenium, browser_id,
                                        provider, oz_page):
    driver = selenium[browser_id]
    prov_rec = oz_page(driver)['go to your files'].providers[provider]

    err_msg = 'provider icon in GO TO YOUR FILES oz panel for ' \
              '"{}" is not green'
    assert prov_rec.is_working(), err_msg.format(provider)


@when(parsers.parse('user of {browser_id} sees that provider named "{provider}" '
                    'in expanded "GO TO YOUR FILES" Onezone panel is not working'))
@then(parsers.parse('user of {browser_id} sees that provider named "{provider}" '
                    'in expanded "GO TO YOUR FILES" Onezone panel is not working'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_provider_not_working_in_oz_panel(selenium, browser_id,
                                            provider, oz_page):
    driver = selenium[browser_id]
    prov_rec = oz_page(driver)['go to your files'].providers[provider]
    err_msg = 'provider icon in GO TO YOUR FILES oz panel for ' \
              '"{}" is not gray'
    assert prov_rec.is_not_working(), err_msg.format(provider)


@when(parsers.parse('user of {browser_id} sees alert with title "{title}" '
                    'on world map in Onezone gui'))
@then(parsers.parse('user of {browser_id} sees alert with title "{title}" '
                    'on world map in Onezone gui'))
@repeat_failed(timeout=WAIT_BACKEND)
def assert_alert_with_title_in_oz(selenium, browser_id, title, oz_page):
    driver = selenium[browser_id]
    err_msg = 'alert title {} does not match {}'
    alert = oz_page(driver)['world map'].message
    assert alert.title.lower() == title.lower(), err_msg.format(alert.title,
                                                                title)


@given(parsers.parse('user of {browser_id} records providers hostname using '
                     'copy hostname button in every provider popup'))
@repeat_failed(timeout=WAIT_FRONTEND)
def record_providers_hostname_oz(selenium, browser_id, oz_page,
                                 tmp_memory, displays, clipboard):
    driver = selenium[browser_id]
    world_map = oz_page(driver)['world map']
    for provider_rec in oz_page(driver)['go to your files'].providers:
        provider_popup = None
        provider_rec_name = provider_rec.name
        provider_popup_name = ''
        now = time.time()

        # condition to prevent looping for eternity
        time_limit = now + 10
        while (provider_popup_name != provider_rec_name) and now < time_limit:
            provider_rec.click()
            with implicit_wait(driver, 0.1, SELENIUM_IMPLICIT_WAIT):
                provider_popup = world_map.get_provider_with_displayed_popup()
            provider_popup_name = provider_popup.name if provider_popup else ''
            now = time.time()
        if now > time and (provider_popup_name != provider_rec_name):
            raise RuntimeError('unable to get popup for '
                               '"{}" provider'.format(provider_rec_name))

        provider_popup.copy_hostname()

        prov_name = clipboard.paste(display=displays[browser_id])
        tmp_memory[provider_popup_name] = prov_name
