"""This module contains definitions of fixtures used in cucumber-like tests
of onedata.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from tests.utils.utils import set_dns, get_oz_cookie
from environment import docker

import os
import subprocess
import tempfile
import pytest


@pytest.fixture(autouse=True)
def skip_by_env(skip_by_env):
    """Autouse fixture defined in tests.conftest
    """
    pass


@pytest.fixture(autouse=True)
def xfail_by_env(xfail_by_env):
    """Autouse fixture defined in tests.conftest
    """
    pass


@pytest.fixture(scope="module")
def open_id(persistent_environment, env_description_file):
    oz_nodes = persistent_environment['oz_worker_nodes']

    set_dns(persistent_environment)

    open_id_host = persistent_environment['appmock_nodes'][0].split('@')[1]
    open_id_ip = docker.inspect(open_id_host)["NetworkSettings"]['IPAddress']

    auth_content = """[{{dropbox, [
            {{auth_module, auth_dropbox}},
            {{app_id, <<"mock_id">>}},
            {{app_secret, <<"mock_secret">>}},
            % Provider specific config
            {{authorize_endpoint, <<"https://{open_id_ip}/1/oauth2/authorize">>}},
            {{access_token_endpoint, <<"https://{open_id_ip}/1/oauth2/token">>}},
            {{user_info_endpoint, <<"https://{open_id_ip}/1/account/info">>}}]
        }}].""".format(open_id_ip=open_id_ip)

    CONFIG_PATH = os.path.join('/root', 'bin', 'node', 'data', 'auth.config')

    # tmp_dir = tempfile.mkdtemp()
    # tmp_file = tempfile.mktemp(dir=tmp_dir)
    tmp_file = tempfile.mktemp()
    # docker.cp(oz_host, CONFIG_PATH, tmp_dir, False)

    with open(tmp_file, 'w') as f:
        f.write(auth_content)

    for oz_node in oz_nodes:
        oz_host = oz_node.split('@')[1]
        cookie = get_oz_cookie(env_description_file, oz_node)

        docker.cp(oz_host, tmp_file, CONFIG_PATH, True)

        subprocess.check_output([os.path.join(UTILS_DIR, 'load_auth_config.escript'),
                                 oz_node, cookie], stderr=subprocess.STDOUT)

    os.remove(tmp_file)
    return open_id_ip

