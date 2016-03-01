import subprocess

from tests.test_common import *
from tests import test_utils
import socket
import time
import os
import ast
import pytest

from environment import docker, env, common, dns, appmock


@pytest.mark.parametrize("script_name", [
    # "appmock_up",
    # "globalregistry_up",
    # "cluster_manager_up",
    "client_up",

])
def test_component_up(script_name):
    # setup
    environment = setup_test(script_name)
    # test
    check_if_node_is_up(environment, script_name)
    # teardown
    teardown_testcase(environment)


# Run the evn_up.py script, capture and parse the output
def setup_test(script_name):
    logdir = get_logdir_name(acceptance_logdir, get_test_name(__file__))
    config_path = test_utils.test_file('_'.join([script_name, 'env.json']))
    uid = common.generate_uid()

    environment = {
        'docker_ids': [],
        'gr_nodes': [],
        'gr_db_nodes': [],
        'cluster_manager_nodes': [],
        'op_worker_nodes': [],
        'cluster_worker_nodes': [],
        'appmock_nodes': [],
        'client_nodes': []
    }

    # Start DNS
    [dns_server], dns_output = dns.maybe_start('auto', uid)
    common.merge(environment, dns_output)

    output = subprocess.check_output([
        os.path.join(docker_dir, up_script(script_name)),
        '-b', bindir(script_name),
        '-d', dns_server,
        '-u', uid,
        # TODO uncomment after adding logs in oneclient
        # '-l', logdir,
        config_path
    ])
    # This is to delete system logs like 'Adding user' etc. from output
    stripped_output = strip_output_logs(output)

    output_dict = ast.literal_eval(stripped_output)
    common.merge(environment, output_dict)
    return environment


# # Clean up removing all dockers created in the test
def teardown_testcase(environment):
    print "called teardown"
    docker.remove(environment['docker_ids'], force=True, volumes=True)


# # Test if the *_up.py script works as expected.
def check_if_node_is_up(env, script_name):
    # Check if number of started dockers is correct
    call_check_function(script_name, env)


def up_script(name):
    return '.'.join([name, 'py'])


def bindir(script_name):
    name, _rest = script_name.split('_', 1)
    if name == "client":
        name = "oneclient"
    return os.path.join(os.getcwd(), name)


def check_appmock_up(env):
    assert 3 == len(env['docker_ids'])
    dns = env['dns']
    # Will throw if the dns address is not legal
    socket.inet_aton(dns)
    # Check connectivity to nodes using the DNS
    # node is in form name@name.timestamp.dev.docker
    for node in env['appmock_nodes']:
        (name, sep, hostname) = node.partition('@')
        ip = test_utils.dns_lookup(hostname, dns)
        assert test_utils.ping(ip)
        assert test_utils.check_http_connectivity(ip, 443, '/test2', 200,
                                              number_of_retries=50)


def check_globalregistry_up(env):
    assert 3 == len(env['docker_ids'])
    dns = env['dns']
    # Will throw if the dns address is not legal
    socket.inet_aton(dns)
    # Check connectivity to nodes using the DNS
    # Check GR nodes
    # gr_node is in form name@name.timestamp.dev.docker
    for gr_node in env['gr_nodes']:
        (gr_name, sep, gr_hostname) = gr_node.partition('@')
        gr_ip = test_utils.dns_lookup(gr_hostname, dns)
        assert test_utils.ping(gr_ip)
        assert test_utils.check_http_connectivity(gr_ip, 443, '/', 200, number_of_retries=50)

    # Check GR DB nodes
    # gr_db_node is in form name@name.timestamp.dev.docker
    for gr_db_node in env['gr_db_nodes']:
        (gr_db_name, sep, gr_db_hostname) = gr_db_node.partition('@')
        gr_db_ip = test_utils.dns_lookup(gr_db_hostname, dns)
        assert test_utils.ping(gr_db_ip)
        assert test_utils.check_http_connectivity(gr_db_ip, 5984, '/_utils/', 200, use_ssl=False, number_of_retries=50)


def check_cluster_manager_up(env):
    assert 2 == len(env['docker_ids'])
    dns = env['dns']
    # Will throw if the dns address is not legal
    socket.inet_aton(dns)
    # Check connectivity to nodes using the DNS
    # Check OP CM nodes
    # cm_node is in form name@name.timestamp.dev.docker
    for cm_node in env['cluster_manager_nodes']:
        (cm_name, sep, cm_hostname) = cm_node.partition('@')
        cm_ip = test_utils.dns_lookup(cm_hostname, dns)
        assert test_utils.ping(cm_ip)


def check_client_up(env):
    assert 2 == len(env['docker_ids'])
    dns = env['dns']
    # Will throw if the dns address is not legal
    socket.inet_aton(dns)
    # Check client nodes
    # oc_node is in form name.timestamp.dev.docker
    for oc_node in env['client_nodes']:
        oc_ip = test_utils.dns_lookup(oc_node, dns)
        assert test_utils.ping(oc_ip)

#########################################################################
# UTILS


def call_check_function(script_name, env):
    function_name = '_'.join(['check', script_name])
    get_function(__name__, function_name)(env)


def get_module(name):
    return sys.modules[name]


def get_function(module_name, function_name):
    return getattr(get_module(module_name), function_name)


def strip_output_logs(output):
    return output.strip().split('\n')[-1]
