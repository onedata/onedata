import subprocess
from tests.test_common import *
from env_test_utils import *
import os
import pytest

from environment import docker, common, dns


@pytest.mark.parametrize("script_name, dockers_num", [
    ("appmock_up", 2),
    ("client_up", 2),
    ("cluster_manager_up", 1),
    ("panel_up", 2),
    ("couchbase_up", 2),
    ("riak_up", 2),
    ("zone_up", 1),
    ("provider_up", {'cm_nodes': 1, 'op_nodes': 2}),
    ("cluster_up", {'cm_nodes': 1, 'cw_nodes': 1})
])
def test_component_up(script_name, dockers_num):
    # setup
    environment = setup_test(script_name)
    # test
    check_if_node_is_up(environment, script_name, dockers_num)
    # teardown
    teardown_testcase(environment)


@pytest.mark.parametrize(
    "env", get_json_files(example_env_dir)
)
def test_example_envs(env):
    output = run_env_up_script("env_up.py", [env])
    teardown_testcase(output)


def test_dns_up():
    output = run_env_up_script("dns_up.py")
    assert test_utils.ping(output['dns'])
    teardown_testcase(output)


def test_s3_up():
    output = run_env_up_script("s3_up.py")
    assert test_utils.ping(output['host_name'].split(":")[0])
    teardown_testcase(output)

# TODO Uncomment this test after integrating with VFS-1599
# def test_ceph_up():
#     output = run_env_up_script("ceph_up.py")
#     assert test_utils.ping(output['host_name'])
#     teardown_testcase(output)


# Run the env_up script, capture and parse the output
def setup_test(script_name):
    config_path = test_utils.test_file('_'.join([script_name, 'env.json']))
    uid = common.generate_uid()
    environment = get_empty_env()

    # Start DNS
    [dns_server], dns_output = dns.maybe_start('auto', uid)
    common.merge(environment, dns_output)

    args = prepare_args(script_name, uid, dns_server, config_path)
    output = run_env_up_script(up_script(script_name), args)
    common.merge(environment, output)

    return environment


# # Clean up removing all dockers created in the test
def teardown_testcase(environment):
    docker.remove(environment['docker_ids'], force=True, volumes=True)


# # Test if the *_up.py script works as expected.
def check_if_node_is_up(env, script_name, dockers_num):
    function_name = '_'.join(['check', script_name])
    get_function(function_name)(env, dockers_num)


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


def prepare_args(script_name, uid, dns_server, config_path):
    args = []
    if is_no_config_script(script_name):
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

    # currently there is no option -l for client_up.py and panel_up.py
    # TODO delete client_up from below list after resolving VFS-1641
    if script_name not in ["client_up", "panel_up"]:
        args.extend([
            '-l', make_logdir(acceptance_logdir, get_test_name(__file__))
        ])
    args.append(config_path)

    if script_name == "cluster_up":
        args.extend([
            '-do', 'cluster_domains'
        ])

    return args


def is_db_script(name):
    return ["riak_up", "couchbase_up"].__contains__(name)


def is_no_config_script(name):
    return ["ceph_up", "s3_up"].__contains__(name)


