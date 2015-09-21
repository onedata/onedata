from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.dir_steps import *
from steps.common import *
from steps.file_steps import *

@scenario(
    '../features/directory_stat.feature',
    'Check file type'
)
def test_type():
    pass


@scenario(
    '../features/directory_stat.feature',
    'Check default access permissions'
)
def test_default_access():
    pass


@scenario(
    '../features/directory_stat.feature',
    'Change access permissions'
)
def test_change_access():
    pass
