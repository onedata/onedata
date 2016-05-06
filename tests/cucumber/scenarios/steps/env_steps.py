"""Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements common functions for handling test environment.
"""
from environment import docker
from pytest_bdd import given, then
from pytest_bdd import parsers

from tests import *


@given("environment is up")
def environment(persistent_environment, request):

    def fin():
        if 'posix' in persistent_environment['storages'].keys():
            for storage_name, storage in persistent_environment['storages']['posix'].items():
                clear_storage(storage['host_path'])

    request.addfinalizer(fin)
    return persistent_environment


@then(parsers.parse('{number:d} nodes are up'))
def check_nodes(environment, number):
    """
    Checks whether environment consists of 'number' nodes.
    """
    assert number == len(environment['docker_ids'])


def clear_storage(storage_path):
    # we don't have permissions to clean storage directory
    # therefore docker with this directory mounted is started
    # (docker has root permissions) and dir is cleaned via docke
    cmd = 'sh -c "rm -rf {path}"'.format(path=os.path.join(storage_path, '*'))
    docker.run(tty=True,
               rm=True,
               interactive=True,
               reflect=[(storage_path, 'rw')],
               image='onedata/worker',
               command=cmd)
