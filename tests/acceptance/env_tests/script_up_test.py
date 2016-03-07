import subprocess
from tests.test_common import *
from env_test_utils import *
import os
import ast
import pytest

from environment import docker, common, dns


@pytest.mark.parametrize("script_name, dockers_num", [
    # ("appmock_up", 2),
    ("client_up", 2),
    # ("cluster_manager_up", 1),
    # ("panel_up", 2),
    # ("couchbase_up", 2),
    # ("riak_up", 2)

    # ??? przechodzi jak nie ma sieci
    # ("globalregistry_up", 1),

    # nie przechodzi, w envie pojawiaja sie IP-ki 2 dnsow sklejone
    # ("provider_up", {'cm_nodes': 1, 'opw_nodes': 2}),
    # ("cluster_up", {'cm_nodes': 1, 'cw_nodes': 1})
])
def test_component_up(script_name, dockers_num):
    # setup
    environment = setup_test(script_name)
    # test
    check_if_node_is_up(environment, script_name, dockers_num)
    # teardown
    teardown_testcase(environment)


def test_dns_up():
    output = subprocess.check_output(os.path.join(docker_dir, "dns_up.py"))
    stripped_output = strip_output_logs(output)
    output_dict = ast.literal_eval(stripped_output)
    assert test_utils.ping(output_dict['dns'])
    teardown_testcase(output_dict)


def test_s3_up():
    output = subprocess.check_output(os.path.join(docker_dir, "s3_up.py"))
    stripped_output = strip_output_logs(output)
    output_dict = ast.literal_eval(stripped_output)
    assert test_utils.ping(output_dict['host_name'].split(":")[0])
    teardown_testcase(output_dict)

# TODO Uncomment this test after integrating with VFS-1599
# def test_ceph_up():
#     output = subprocess.check_output(os.path.join(docker_dir, "ceph_up.py"))
#     stripped_output = strip_output_logs(output)
#     output_dict = ast.literal_eval(stripped_output)
#     assert test_utils.ping(output_dict['host_name'])
#     teardown_testcase(output_dict)

# Run the evn_up.py script, capture and parse the output
def setup_test(script_name):
    config_path = test_utils.test_file('_'.join([script_name, 'env.json']))
    uid = common.generate_uid()
    environment = get_empty_env()

    # Start DNS
    [dns_server], dns_output = dns.maybe_start('auto', uid)
    common.merge(environment, dns_output)

    cmd = prepare_cmd(script_name, uid, dns_server, config_path)
    output = subprocess.check_output(cmd)
    # # Below line deletes system logs like 'Adding user' etc. from output
    stripped_output = strip_output_logs(output)
    output_dict = ast.literal_eval(stripped_output)
    common.merge(environment, output_dict)

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
        'gr_nodes': [],
        'gr_db_nodes': [],
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
    elif name == "client":
        name = "oneclient"
    elif name == "panel":
        name = "onepanel"
    return ['-b', os.path.join(os.getcwd(), name)]


def prepare_cmd(script_name, uid, dns_server, config_path):
    cmd = [os.path.join(docker_dir, up_script(script_name))]
    if is_no_config_script(script_name):
        return cmd

    cmd.extend([
        '-d', dns_server,
        '-u', uid
    ])
    # couchbase_up.py and riak_up.py don't take config as argument
    if is_db_script(script_name):
        return cmd

    if script_name != "env_up":
        cmd.extend(bindir_options(script_name)),

    # currently there is no option -l for client_up.py and panel_up.py
    # TODO delete client_up from below list after resolving VFS-1641
    if script_name not in ["client_up", "panel_up"]:
        cmd.extend([
            '-l', get_logdir_name(acceptance_logdir, get_test_name(__file__))
        ])
    cmd.append(config_path)

    if script_name == "cluster_up":
        cmd.extend([
            '-do', 'cluster_domains'
        ])

    return cmd


def is_db_script(name):
    return ["riak_up", "couchbase_up"].__contains__(name)


def is_no_config_script(name):
    return ["ceph_up", "s3_up"].__contains__(name)
