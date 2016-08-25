"""This module contains acceptance tests of scripts that bring up dockerized
test environment.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from tests.utils.utils import run_env_up_script
from tests.utils.path_utils import (config_file, get_file_name, make_logdir,
                                    get_json_files, get_function)
from tests.utils.net_utils import ping
import test_utils
from environment import docker, common, dns

import pytest


@pytest.mark.parametrize("script_name, dockers_num", [
    ("appmock_up", 2),
    ("client_up", 2),
    ("cluster_manager_up", 1),
    ("cluster_up", {'cm_nodes': 1, 'cw_nodes': 1}),
    ("panel_up", 2),
    ("provider_up", {'cm_nodes': 1, 'op_nodes': 2}),
    ("zone_up", 1),
    ("couchbase_up", 2),
    ("riak_up", 2),
])
def test_component_up(script_name, dockers_num):
    # setup
    environment = setup_test(script_name)
    # test
    check_if_node_is_up(environment, script_name, dockers_num)
    # teardown
    teardown_testcase(environment)


@pytest.mark.parametrize(
    "env", get_json_files(EXAMPLE_ENV_DIR)
)
def test_example_envs(env):
    logdir = make_logdir(ENV_UP_LOGDIR,
                         os.path.join(get_file_name(__file__),
                                      env.split(os.path.sep)[-1]))
    output = run_env_up_script("env_up.py", config=env, logdir=logdir,
                               skip=False)
    teardown_testcase(output)


def test_dns_up():
    output = run_env_up_script("dns_up.py", skip=False)
    assert ping(output['dns'])
    teardown_testcase(output)


def test_s3_up():
    output = run_env_up_script("s3_up.py", skip=False)
    assert ping(output['host_name'].split(":")[0])
    teardown_testcase(output)


def test_ceph_up():
    output = run_env_up_script("ceph_up.py", skip=False)
    assert ping(output['host_name'])
    teardown_testcase(output)


# Run the env_up script, capture and parse the output
def setup_test(script_name):
    config_path = None
    if not is_no_args_script(script_name) and not is_db_script(script_name):
        config_path = config_file('_'.join([script_name, 'env.json']))
    uid = common.generate_uid()
    environment = get_empty_env()

    # Start DNS
    [dns_server], dns_output = dns.maybe_start('auto', uid)
    common.merge(environment, dns_output)

    args = prepare_args(script_name, uid, dns_server)

    logdir = make_logdir(ENV_UP_LOGDIR,
                         os.path.join(get_file_name(__file__), script_name))

    output = run_env_up_script(up_script(script_name),
                               config=config_path,
                               logdir=logdir if not is_no_logdir_script(script_name) else None,
                               args=args, skip=False)
    common.merge(environment, output)

    return environment


# # Clean up removing all dockers created in the test
def teardown_testcase(onedata_environment):
    docker.remove(onedata_environment['docker_ids'], force=True, volumes=True)


# # Test if the *_up.py script works as expected.
def check_if_node_is_up(env, script_name, dockers_num):
    function_name = '_'.join(['check', script_name])
    get_function(test_utils, function_name)(env, dockers_num)


################################################################################
# # INTERNAL FUNCTIONS
################################################################################

def get_empty_env():
    return {
        'docker_ids': [],
        'oz_nodes': [],
        'oz_db_nodes': [],
        'cluster_manager_nodes': [],
        'op_worker_nodes': [],
        'cluster_worker_nodes': [],
        'appmock_nodes': [],
        'client_nodes': [],
    }


def up_script(name):
    return '.'.join([name, 'py'])


def bindir_options(script_name):
    name = script_name.replace("_up", "")
    if name == "provider":
        return [
            '-bw', os.path.join(os.getcwd(), 'op_worker'),
            '-bcm', os.path.join(os.getcwd(), 'cluster_manager')
        ]
    elif name == "cluster":
        return [
            '-bw', os.path.join(os.getcwd(), 'cluster_worker'),
            '-bcm', os.path.join(os.getcwd(), 'cluster_manager')
        ]
    elif name == "zone":
        return [
            '-boz', os.path.join(os.getcwd(), 'oz_worker'),
            '-bcm', os.path.join(os.getcwd(), 'cluster_manager')
        ]
    elif name == "client":
        name = "oneclient"
    elif name == "panel":
        name = "onepanel"
    return ['-b', os.path.join(os.getcwd(), name)]


# def prepare_args(script_name, uid, dns_server, config_path):
def prepare_args(script_name, uid, dns_server):
    args = []
    if is_no_args_script(script_name):
        return args

    args.extend([
        '-d', dns_server,
        '-u', uid
    ])
    # couchbase_up.py and riak_up.py don't take config as argument
    if is_db_script(script_name):
        return args

    if script_name != "env_up":
        args.extend(bindir_options(script_name)),

    if script_name == "cluster_up":
        args.extend([
            '-do', 'cluster_domains'
        ])

    return args


def is_db_script(name):
    return ["riak_up", "couchbase_up"].__contains__(name)


def is_no_args_script(name):
    return ["ceph_up", "s3_up"].__contains__(name)


def is_no_logdir_script(name):
    return is_no_args_script(name) or is_db_script(name) or name == "panel_up"