"""Steps used in provider page in onepanel"""

from pytest_bdd import when, then, parsers

from tests.gui.conftest import WAIT_FRONTEND
from tests.gui.utils.generic import repeat_failed, transform


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@when(parsers.re('user of (?P<browser_id>.*?) sees that (?P<attr>ID|'
                 'Provider name|Redirection point|URLs|Latitude|Longitude) '
                 'attribute of provider has value of "(?P<val>.*?)" '
                 'in Provider panel'))
@then(parsers.re('user of (?P<browser_id>.*?) sees that (?P<attr>ID|'
                 'Provider name|Redirection point|URLs|Latitude|Longitude) '
                 'attribute of provider has value of "(?P<val>.*?)" '
                 'in Provider panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_value_of_provider_attribute(selenium, browser_id,
                                          attr, val, onepanel):
    info = onepanel(selenium[browser_id]).content.provider.details
    displayed_val = getattr(info, transform(attr))
    assert displayed_val == val, \
        'displayed {} instead of expected {}'.format(displayed_val, val)


@when(parsers.parse('user of {browser_id} sees that Redirection point '
                    'provider attribute has ip value of "{host}" provider '
                    'in Provider panel'))
@then(parsers.parse('user of {browser_id} sees that Redirection point '
                    'provider attribute has ip value of "{host}" provider '
                    'in Provider panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_assert_value_of_provider_redirection_point(selenium, browser_id,
                                                  host, hosts, onepanel):
    ip = 'https://{}'.format(hosts['oneprovider'][host])
    info = onepanel(selenium[browser_id]).content.provider.details
    displayed_ip = info.redirection_point
    assert displayed_ip == ip, \
        'displayed {} instead of expected {}'.format(displayed_ip, ip)


@when(parsers.re('user of (?P<browser_id>.*?) types "(?P<val>.*?)" to '
                 '(?P<attr>Provider name|Redirection point|Latitude|Longitude) '
                 'input box in modify provider details form in Provider panel'))
@then(parsers.re('user of (?P<browser_id>.*?) types "(?P<val>.*?)" to '
                 '(?P<attr>Provider name|Redirection point|Latitude|Longitude) '
                 'input box in modify provider details form in Provider panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_val_to_in_box_in_provider_details_form(selenium, browser_id,
                                                   val, attr, onepanel):
    setattr(onepanel(selenium[browser_id]).content.provider.form,
            transform(attr), val)


@when(parsers.parse('user of {browser_id} types "{host}" provider ip to '
                    'Redirection point input box in modify provider details '
                    'form in Provider panel'))
@then(parsers.parse('user of {browser_id} types "{host}" provider ip to '
                    'Redirection point input box in modify provider details '
                    'form in Provider panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_type_val_to_in_box_in_provider_details_form(selenium, browser_id,
                                                   host, hosts, onepanel):
    form = onepanel(selenium[browser_id]).content.provider.form
    form.redirection_point = 'https://{}'.format(hosts['oneprovider'][host])


@when(parsers.parse('user of {browser_id} clicks on Modify provider details '
                    'button in provider details form in Provider panel'))
@then(parsers.parse('user of {browser_id} clicks on Modify provider details '
                    'button in provider details form in Provider panel'))
@repeat_failed(timeout=WAIT_FRONTEND)
def wt_click_on_btn_in_modify_provider_detail_form(selenium, browser_id, onepanel):
    onepanel(selenium[browser_id]).content.provider.form.modify_provider_details()
