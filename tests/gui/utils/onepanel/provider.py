"""Utils to facilitate provider operations in op panel GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (Label, NamedButton,
                                               Input, Button, WebItem)


class ProviderDetailsCommon(PageObject):
    id = Label('.field-main-id')
    urls = Label('.field-main-urls')


class ProviderDetails(ProviderDetailsCommon):
    provider_name = Label('.field-main-name')
    redirection_point = Label('.field-main-redirectionPoint')
    latitude = Label('.field-main-geoLatitude')
    longitude = Label('.field-main-geoLongitude')


class ModifyProviderDetailsForm(ProviderDetailsCommon):
    provider_name = Input('input.field-main-name')
    redirection_point = Input('input.field-main-redirectionPoint')
    latitude = Input('input.field-main-geoLatitude')
    longitude = Input('input.field-main-geoLongitude')
    modify_provider_details = NamedButton('button',
                                          text='Modify provider details')


class ProviderContentPage(PageObject):
    details = WebItem('.provider-registration-form', cls=ProviderDetails)
    form = WebItem('.provider-registration-form', cls=ModifyProviderDetailsForm)
    modify_provider_details = NamedButton('.btn-modify-provider',
                                          text='Modify provider details')
    cancel_modifying = NamedButton('.btn-modify-provider',
                                   text='Cancel modifying')
    deregister_provider = Button('.btn-deregister-provider')
