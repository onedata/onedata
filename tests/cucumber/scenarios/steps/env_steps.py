"""Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements common functions for handling test environment.
"""

from tests.test_common import (make_logdir, run_env_up_script, env_name,
                               cucumber_logdir, default_cucumber_env_dir,
                               get_json_files)
from environment import common, docker

import pytest
from pytest_bdd import parsers
from pytest_bdd import given, then

import os


@pytest.fixture(scope="module",
                # params=get_json_files(default_cucumber_env_dir, relative=True))
                params=["env3.json"])
def env_description_file(request):
    """NOTE: This fixture must be overridden in every test module. As params
    for overridden fixture you must specify .json files with description
    of test environment for which you want tests from given module to be
    started.
    """
    absolute_path = os.path.join(default_cucumber_env_dir, request.param)
    return absolute_path


@pytest.fixture(scope="module")
def persistent_environment(request, context, env_description_file):
    """
    Sets up environment and returns environment description.
    """
    curr_path = os.path.dirname(os.path.abspath(__file__))
    env_path = os.path.join(curr_path, '..', '..', 'environments',
                            env_description_file)

    feature_name = request.module.__name__.split('.')[-1]
    logdir = make_logdir(cucumber_logdir, os.path
                         .join(env_name(env_description_file), feature_name))
    env_desc = run_env_up_script("env_up.py", config=env_path, logdir=logdir)

    def fin():
        docker.remove(request.environment['docker_ids'],
                      force=True,
                      volumes=True)

    request.addfinalizer(fin)
    request.environment = env_desc
    return env_desc


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
    cmd = 'sh -c "rm -r {path}"'.format(path=os.path.join(storage_path, '*'))
    docker.run(tty=True,
               rm=True,
               interactive=True,
               reflect=[(storage_path, 'rw')],
               image='onedata/worker',
               command=cmd)
