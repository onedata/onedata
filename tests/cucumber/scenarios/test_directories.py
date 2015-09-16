from pytest_bdd import scenario

from steps.env_steps import *
from steps.auth_steps import *
from steps.dir_steps import *
from steps.common import *


@scenario(
    '../features/directory_CRUD.feature',
    'Create directory'
)
def test_create():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Rename directory'
)
def test_rename():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Delete directory'
)
def test_delete():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Child directories'
)
def test_children():
    pass


@scenario(
    '../features/directory_CRUD.feature',
    'Double creation'
)
def test_double():
    pass
