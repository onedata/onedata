"""
Define fixtures used in web GUI acceptance/behavioral tests.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.utils import set_dns
from tests.utils.path_utils import env_file
from pytest import fixture
import tests
import pytest
import re

import sys

is_base_url_provided = re.match(r'.*--base-url=.*', ' '.join(sys.argv))

if not is_base_url_provided:
    # FIXME: blocking usage of environment fixture
    raise Exception('do not want to use env yet')
    @fixture(scope='module')
    def base_url(onedata_environment):
        """
        When --base-url is not provided - set up an environment and get a OZ host.
        Assume, that protocol is always HTTPS, so return base_url: https://<oz_host>
        """
        set_dns(onedata_environment)
        oz_host = re.match(r'worker@node\d*\.(.*)', onedata_environment["oz_worker_nodes"][0]).groups(0)[0]
        return "https://{oz_host}".format(oz_host=oz_host)

# In GUI tests use Onedata environment defined for GUI
@pytest.fixture(scope="module",
                params=["single_oz_single_op_env"])
def env_description_file(request):
    return env_file(tests.GUI_ENV_DIR, request.param)