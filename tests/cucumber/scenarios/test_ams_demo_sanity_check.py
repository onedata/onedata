"""
Author: Tomasz Lichon
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite Amsterdam demo setup.
"""

from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.multi_dir_steps import *
from steps.multi_file_steps import *
from steps.multi_reg_file_steps import *
from steps.common import *

#
# Local/Local
#

@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Local/Local space - create files and see them on external provider'
)
def test_shared_space_create_and_list():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Local/Local space - create empty file and read it on external provider'
)
def test_shared_space_create_empty_and_read():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Local/Local space - write to file and check size on remote provider'
)
def test_shared_space_write_and_check_size():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Local/Local space - write to file and read on remote provider'
)
def test_shared_space_write_and_read():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Local/Local space - big file transfer with MD5 check'
)
def test_shared_space_big_transfer_and_md5_check():
    pass

#
# Local/Proxy
#

@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Local/Proxy space - create files and see them on external provider'
)
def test_local_proxy_space_create_and_list():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Local/Proxy space - create empty file and read it on external provider'
)
def test_local_proxy_space_create_empty_and_read():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Local/Proxy space - write to file and check size on remote provider'
)
def test_local_proxy_space_write_and_check_size():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Local/Proxy space - write to file and read on remote provider'
)
def test_local_proxy_space_write_and_read():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Local/Proxy space - big file transfer with MD5 check'
)
def test_local_proxy_space_big_transfer_and_md5_check():
    pass

#
# Proxy/Local
#

@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Proxy/Local space - create files and see them on external provider'
)
def test_proxy_local_space_create_and_list():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Proxy/Local space - create empty file and read it on external provider'
)
def test_proxy_local_space_create_empty_and_read():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Proxy/Local space - write to file and check size on remote provider'
)
def test_proxy_local_space_write_and_check_size():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Proxy/Local space - write to file and read on remote provider'
)
def test_proxy_local_space_write_and_read():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Proxy/Local space - big file transfer with MD5 check'
)
def test_proxy_local_space_big_transfer_and_md5_check():
    pass

#
# Proxy/Proxy
#

@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Proxy/Proxy space - create files and see them on external provider'
)
def test_proxy_proxy_space_create_and_list():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Proxy/Proxy space - create empty file and read it on external provider'
)
def test_proxy_proxy_space_create_empty_and_read():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Proxy/Proxy space - write to file and check size on remote provider'
)
def test_proxy_proxy_space_write_and_check_size():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Proxy/Proxy space - write to file and read on remote provider'
)
def test_proxy_proxy_space_write_and_read():
    pass


@scenario(
    '../features/ams_demo_sanity_check.feature',
    'Proxy/Proxy space - big file transfer with MD5 check'
)
def test_proxy_proxy_space_big_transfer_and_md5_check():
    pass

#
# Other
#

@scenario(
    '../features/ams_demo_sanity_check.feature',
    'List spaces dir'
)
def test_list_spaces_dir():
    pass

