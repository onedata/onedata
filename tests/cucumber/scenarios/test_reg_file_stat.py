from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.dir_steps import *
from steps.common import *
from steps.file_steps import *
from steps.reg_file_steps import *

@scenario(
    '../features/reg_file_stat.feature',
    'Check file type'
)
def test_type():
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Check default access permissions'
)
def test_default_access():
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Change access permissions'
)
def test_change_access():
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Increase regular file size'
)
def test_increase_size():
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Decrease regular file size'
)
def test_decrease_size():
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Access time'
)
def test_access_time():
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Modification time'
)
def test_modification_time():
    pass


@scenario(
    '../features/reg_file_stat.feature',
    'Status change time'
)
def test_stat_change_time():
    pass
