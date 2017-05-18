"""Utils and fixtures to facilitate provider operations in op panel GUI."""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (Label, NamedButton,
                                               Input, WebItem)

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class ProviderDescription(PageObject):
    id = Label('.field-id')
    urls = Label('.field-urls')


class ProviderDetails(ProviderDescription):
    provider_name = Label('.field-name')
    redirection_point = Label('.field-redirectionPoint')
    latitude = Label('.field-geoLatitude')
    longitude = Label('.field-geoLongitude')

    def __str__(self):
        return 'Provider details in {}'.format(self.parent)


class ProviderForm(ProviderDescription):
    provider_name = Input('input.field-name')
    redirection_point = Input('input.field-redirectionPoint')
    latitude = Input('input.field-geoLatitude')
    longitude = Input('input.field-geoLongitude')
    modify_provider_details = NamedButton('button',
                                          text='Modify provider details')

    def __str__(self):
        return 'Provider details modify form in {}'.format(self.parent)


class ProviderContentPage(PageObject):
    details = WebItem('.provider-registration-form', cls=ProviderDetails)
    form = WebItem('.provider-registration-form', cls=ProviderForm)
    modify_provider_details = NamedButton('.btn-modify-provider',
                                          text='Modify provider details')
    cancel_modifying = NamedButton('.btn-modify-provider',
                                   text='Cancel modifying')
    deregister_provider = NamedButton('.btn-deregister-provider',
                                      text='Deregister provider')

    def __str__(self):
        return 'Provider page in {}'.format(self.parent)
