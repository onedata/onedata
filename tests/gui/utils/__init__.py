"""Utils for GUI tests
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


from .oneservices.cdmi import CDMIClient
from .common.common import OnePage
from .onepanel import Onepanel
from .onezone.login_page import OnezoneLoginPage
from .onepanel.login_page import OnepanelLoginPage
from .onezone import OZLoggedIn
from .oneprovider import OPLoggedIn
from .oneprovider.shares.public_share import PublicShareView
from .common.modals import Modals
from .common.popups import Popups
