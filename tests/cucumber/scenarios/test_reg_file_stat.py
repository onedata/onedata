"""
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Test suite for reading/changing  metadata of regular files in onedata.
"""

from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.dir_steps import *
from steps.common import *
from steps.file_steps import *
from steps.reg_file_steps import *

# TODO below tests should be uncommented after integrating with VFS-1218 in op_worker
#  and VFS-1235 in oneclient
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Check file type when empty'
# )
# def test_type_empty():
#     pass
#
#
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Check file type when non-empty'
# )
# def test_type():
#     pass
#
#
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Check default access permissions'
# )
# def test_default_access():
#     pass
#
#
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Change access permissions'
# )
# def test_change_access():
#     pass
#
#
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Increase regular file size'
# )
# def test_increase_size():
#     pass
#
#
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Decrease regular file size'
# )
# def test_decrease_size():
#     pass
#
#
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Timestamps at creation'
# )
# def test_timestamp():
#     pass
#
#
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Access time'
# )
# def test_access_time():
#     pass
#
#
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Modification time'
# )
# def test_modification_time():
#     pass
#
#
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Status-change time when changing mode'
# )
# def test_stat_change_time_chmod():
#     pass
#
#
# @scenario(
#     '../features/reg_file_stat.feature',
#     'Status-change time when renaming'
# )
# def test_stat_change_time_mv():
#     pass
