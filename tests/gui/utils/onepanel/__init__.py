"""Utils and fixtures to facilitate operations on Onepanel of zone web GUI.
"""
from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import WebItem, Label

from tests.gui.utils.common.common import OnePage, BaseContent
from .clusters import Clusters, WelcomePage
from .deployment import Deployment
from .nodes import NodesContentPage
from .provider import ProviderContentPage
from .spaces import SpacesContentPage
from .storages import StorageContentPage


__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class Sidebar(PageObject):
    title = Label('.col-title')
    clusters = WebItem('.one-sidebar', cls=Clusters)

    def __str__(self):
        return '{} sidebar in {}'.format(self.title, self.parent)


class Content(BaseContent):
    _main_content = '.main-content'
    welcome_page = WebItem(_main_content, cls=WelcomePage)
    deployment = WebItem(_main_content, cls=Deployment)
    nodes = WebItem(_main_content, cls=NodesContentPage)
    provider = WebItem(_main_content, cls=ProviderContentPage)
    storages = WebItem(_main_content, cls=StorageContentPage)


class Onepanel(OnePage):
    sidebar = WebItem('#col-sidebar', cls=Sidebar)
    content = WebItem('.col-content', cls=Content)

    def __str__(self):
        return 'Onepanel page'


class OZPanel(Onepanel):
    def __str__(self):
        return 'Onepanel zone page'


class OPPanel(Onepanel):
    def __str__(self):
        return 'Onepanel provider page'
