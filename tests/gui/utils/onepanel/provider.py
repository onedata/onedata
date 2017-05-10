"""Utils and fixtures to facilitate provider operations in op panel GUI."""

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import (Label, NamedButton,
                                               Input, WebItem)

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class ProviderDescription(PageObject):
    title = Label('.form-title')
    description = Label('.form-description')
    id = Label('.field-id')
    redirection_point = Label('.field-redirectionPoint')


class ProviderDetails(ProviderDescription):
    provider_name = Label('.field-name')
    urls = Label('.field-urls')
    latitude = Label('.field-geoLatitude')
    longitude = Label('.field-geoLongitude')

    def __str__(self):
        return 'Provider details in {}'.format(self.parent)


class ProviderForm(ProviderDescription):
    provider_name = Input('input.field-name')
    urls = Input('input.field-urls')
    latitude = Input('input.field-geoLatitude')
    longitude = Input('input.field-geoLongitude')
    modify_provider_details = NamedButton('button',
                                          text='Modify provider details')

    def __str__(self):
        return 'Provider details modify form in {}'.format(self.parent)


class ProviderContentPage(PageObject):
    provider_details = WebItem('.content-row', cls=ProviderDetails)
    modify_form = WebItem('.content-row', cls=ProviderForm)
    modify_provider_details = NamedButton('.btn-modify-provider',
                                          text='Modify provider details')
    cancel_modifying = NamedButton('.btn-modify-provider',
                                   text='Cancel modifying')
    deregister_provider = NamedButton('.btn-deregister-provider',
                                      text='Deregister provider')

    def __str__(self):
        return 'Provider page in {}'.format(self.parent)
