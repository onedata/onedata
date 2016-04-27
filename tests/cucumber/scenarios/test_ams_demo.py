"""
Author: Tomasz Lichon
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite Amsterdam demo setup.
"""

from pytest_bdd import scenario

from tests.test_common import custom_cucumber_env_dir

from steps.env_steps import *
from steps.auth_steps import *
from steps.multi_dir_steps import *
from steps.multi_file_steps import *
from steps.multi_reg_file_steps import *
from steps.common import *

@pytest.fixture(scope="module", params=["ams_demo_env.json"])
def env_description_file(request):
    absolute_path = os.path.join(custom_cucumber_env_dir, request.param)
    return absolute_path


#
# Local/Local
#

@scenario(
    '../features/ams_demo.feature',
    'Local/Local space - create files and see them on external provider'
)
def test_shared_space_create_and_list(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Local/Local space - create empty file and read it on external provider'
)
def test_shared_space_create_empty_and_read(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Local/Local space - write to file and check size on remote provider'
)
def test_shared_space_write_and_check_size(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Local/Local space - write to file and read on remote provider'
)
def test_shared_space_write_and_read(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Local/Local space - big file transfer with MD5 check'
)
def test_shared_space_big_transfer_and_md5_check(env_description_file):
    pass

#
# Local/Proxy
#

@scenario(
    '../features/ams_demo.feature',
    'Local/Proxy space - create files and see them on external provider'
)
def test_local_proxy_space_create_and_list(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Local/Proxy space - create empty file and read it on external provider'
)
def test_local_proxy_space_create_empty_and_read(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Local/Proxy space - write to file and check size on remote provider'
)
def test_local_proxy_space_write_and_check_size(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Local/Proxy space - write to file and read on remote provider'
)
def test_local_proxy_space_write_and_read(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Local/Proxy space - big file transfer with MD5 check'
)
def test_local_proxy_space_big_transfer_and_md5_check(env_description_file):
    pass

#
# Proxy/Local
#

@scenario(
    '../features/ams_demo.feature',
    'Proxy/Local space - create files and see them on external provider'
)
def test_proxy_local_space_create_and_list(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Proxy/Local space - create empty file and read it on external provider'
)
def test_proxy_local_space_create_empty_and_read(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Proxy/Local space - write to file and check size on remote provider'
)
def test_proxy_local_space_write_and_check_size(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Proxy/Local space - write to file and read on remote provider'
)
def test_proxy_local_space_write_and_read(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Proxy/Local space - big file transfer with MD5 check'
)
def test_proxy_local_space_big_transfer_and_md5_check(env_description_file):
    pass

#
# Proxy/Proxy
#

@scenario(
    '../features/ams_demo.feature',
    'Proxy/Proxy space - create files and see them on external provider'
)
def test_proxy_proxy_space_create_and_list(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Proxy/Proxy space - create empty file and read it on external provider'
)
def test_proxy_proxy_space_create_empty_and_read(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Proxy/Proxy space - write to file and check size on remote provider'
)
def test_proxy_proxy_space_write_and_check_size(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Proxy/Proxy space - write to file and read on remote provider'
)
def test_proxy_proxy_space_write_and_read(env_description_file):
    pass


@scenario(
    '../features/ams_demo.feature',
    'Proxy/Proxy space - big file transfer with MD5 check'
)
def test_proxy_proxy_space_big_transfer_and_md5_check(env_description_file):
    pass

#
# Other
#

@scenario(
    '../features/ams_demo.feature',
    'List spaces dir'
)
def test_list_spaces_dir(env_description_file):
    pass

