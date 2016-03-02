import subprocess

from tests.test_common import *
from tests import test_utils
from env_test_utils import *
import socket
import time
import os
import ast
import pytest

from environment import docker, common, dns


@pytest.mark.parametrize("script_name, dockers_num", [
    ("appmock_up", 2),
    ("client_up", 1),
    ("cluster_manager_up", 1),
    ("panel_up", 2),
    ("couchbase_up", 2),
    ("riak_up", 2)

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


def check_s3_up():
    output = subprocess.check_output(os.path.join(docker_dir, "s3_up.py"))
    stripped_output = strip_output_logs(output)
    output_dict = ast.literal_eval(stripped_output)
    assert test_utils.ping(output_dict['host_name'].split(":")[0])


# TODO Uncomment this test after integrating with VFS-1599
# def test_ceph_up():
#     output = subprocess.check_output(os.path.join(docker_dir, "ceph_up.py"))
#     stripped_output = strip_output_logs(output)
#     output_dict = ast.literal_eval(stripped_output)
#     assert test_utils.ping(output_dict['host_name'])


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


# def check_appmock_up(env, dockers_num):
#     key = 'appmock_nodes'
#     assert dockers_num == len(env[key])
#     dns = env['dns']
#     # Will throw if the dns address is not legal
#     socket.inet_aton(dns)
#     # Check connectivity to nodes using the DNS
#     # node is in form name@name.timestamp.dev.docker
#     for node in env[key]:
#         (name, sep, hostname) = node.partition('@')
#         ip = test_utils.dns_lookup(hostname, dns)
#         assert test_utils.ping(ip)
#         assert test_utils.check_http_connectivity(ip, 443, '/test2', 200,
#                                                   number_of_retries=50)
#
#
# def check_client_up(env, dockers_num):
#     key = 'client_nodes'
#     assert dockers_num == len(env[key])
#     dns = env['dns']
#     # Will throw if the dns address is not legal
#     socket.inet_aton(dns)
#     # Check client nodes
#     # oc_node is in form name.timestamp.dev.docker
#     for oc_node in env[key]:
#         oc_ip = test_utils.dns_lookup(oc_node, dns)
#         assert test_utils.ping(oc_ip)
#
#
# def check_cluster_manager_up(env, dockers_num):
#     key = 'cluster_manager_nodes'
#     assert dockers_num == len(env[key])
#     dns = env['dns']
#     # Will throw if the dns address is not legal
#     socket.inet_aton(dns)
#     # Check connectivity to nodes using the DNS
#     # Check OP CM nodes
#     # cm_node is in form name@name.timestamp.dev.docker
#     for cm_node in env[key]:
#         (cm_name, sep, cm_hostname) = cm_node.partition('@')
#         cm_ip = test_utils.dns_lookup(cm_hostname, dns)
#         assert test_utils.ping(cm_ip)
#
#
# def check_cluster_up(env, dockers_num):
#     check_cluster_manager_up(env, dockers_num['cm_nodes'])
#     check_cluster_worker_up(env, dockers_num['cw_nodes'])
#
#
# def check_cluster_worker_up(env, dockers_num):
#     key = 'op_worker_nodes'
#     assert dockers_num == len(env[key])
#     dns = env['dns']
#     # Will throw if the dns address is not legal
#     socket.inet_aton(dns)
#     for node in env[key]:
#         (name, sep, hostname) = node.partition('@')
#         ip = test_utils.dns_lookup(hostname, dns)
#         assert test_utils.ping(ip)
#         assert test_utils.check_http_connectivity(ip, 6666, '/nagios', 200,
#                                                   use_ssl=False,
#                                                   number_of_retries=50)
#
#
# def check_globalregistry_up(env, dockers_num):
#     key = 'gr_nodes'
#     assert dockers_num == len(env[key])
#     dns = env['dns']
#     # Will throw if the dns address is not legal
#     socket.inet_aton(dns)
#     # Check connectivity to nodes using the DNS
#     # Check GR nodes
#     # gr_node is in form name@name.timestamp.dev.docker
#     for gr_node in env[key]:
#         (gr_name, sep, gr_hostname) = gr_node.partition('@')
#         gr_ip = test_utils.dns_lookup(gr_hostname, dns)
#         assert test_utils.ping(gr_ip)
#         assert test_utils.check_http_connectivity(gr_ip, 443, '/', 200,
#                                                   number_of_retries=50)
#
#     # Check GR DB nodes
#     # gr_db_node is in form name@name.timestamp.dev.docker
#     for gr_db_node in env['gr_db_nodes']:
#         (gr_db_name, sep, gr_db_hostname) = gr_db_node.partition('@')
#         gr_db_ip = test_utils.dns_lookup(gr_db_hostname, dns)
#         assert test_utils.ping(gr_db_ip)
#         assert test_utils.check_http_connectivity(gr_db_ip, 5984, '/_utils/',
#                                                   200, use_ssl=False,
#                                                   number_of_retries=50)
#
#
# def check_provider_up(env, dockers_num):
#     check_cluster_manager_up(env, dockers_num['cm_nodes'])
#     check_provider_worker_up(env, dockers_num['op_worker'])
#
#
# def check_provider_worker_up(env, dockers_num):
#     key = 'op_worker_nodes'
#     assert dockers_num == len(env[key])
#     dns = env['dns']
#     # Will throw if the dns address is not legal
#     socket.inet_aton(dns)
#     for w_node in env[key]:
#         (w_name, sep, w_hostname) = w_node.partition('@')
#         w_ip = test_utils.dns_lookup(w_hostname, dns)
#         assert test_utils.ping(w_ip)
#         assert test_utils.check_http_connectivity(w_ip, 6666, '/nagios', 200,
#                                                   use_ssl=False,
#                                                   number_of_retries=50)
#
#
# def check_riak_up(env, dockers_num):
#     key = 'riak_nodes'
#     assert dockers_num == len(env[key])
#     dns = env['dns']
#     # Will throw if the dns address is not legal
#     socket.inet_aton(dns)
#     for node in env[key]:
#         ip = test_utils.dns_lookup(node, dns)
#         assert test_utils.ping(ip)
#
#
# def check_couchbase_up(env, dockers_num):
#     key = 'couchbase_nodes'
#     assert dockers_num == len(env[key])
#     dns = env['dns']
#     # Will throw if the dns address is not legal
#     socket.inet_aton(dns)
#
#     for node in env[key]:
#         ip = test_utils.dns_lookup(node, dns)
#         assert test_utils.ping(ip)
#
#
# def check_panel_up(env, dockers_num):
#     key = 'onepanel_nodes'
#     assert dockers_num == len(env[key])
#     dns = env['dns']
#     # Will throw if the dns address is not legal
#     socket.inet_aton(dns)
#
#     for node in env[key]:
#         (name, sep, hostname) = node.partition('@')
#         ip = test_utils.dns_lookup(hostname, dns)
#         assert test_utils.ping(ip)


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


# def get_module(name):
#     return sys.modules[name]


# def get_function(module_name, function_name):
#     return getattr(get_module(module_name), function_name)


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
