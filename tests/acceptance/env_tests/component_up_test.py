import subprocess

from tests.test_common import *
from tests import test_utils
import socket
import time
import os
import ast
import pytest

from environment import docker, env, common, dns, appmock


@pytest.mark.parametrize("component_name, expected", [
    ("appmock", {'dockers_num': 3, 'test_path': '/test2'}),
    ("globalregistry", {'dockers_num': 3, 'test_path': '/'})
])
def test_component_up(component_name, expected):
    # setup
    environment = setup_test(component_name)

    # test
    check_if_node_is_up(environment, component_name, expected)

    # teardown
    teardown_testcase(environment)


# Run the evn_up.py script, capture and parse the output
def setup_test(component_name):
    logdir = get_logdir_name(acceptance_logdir, get_test_name(__file__))
    config_path = test_utils.test_file(os.path.join(component_name, 'env.json'))
    uid = common.generate_uid()

    output = {
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
    common.merge(output, dns_output)

    am_output = subprocess.check_output([
        os.path.join(docker_dir, up_script(component_name)),
        '-b', bindir(component_name),
        '-d', dns_server,
        '-u', uid,
        '-l', logdir,
        config_path
    ])

    am_output = ast.literal_eval(am_output)
    common.merge(output, am_output)
    return output


# # Clean up removing all dockers created in the test
def teardown_testcase(environment):
    print "called teardown"
    docker.remove(environment['docker_ids'], force=True, volumes=True)


# # Test if the *_up.py script works as expected.
def check_if_node_is_up(env, component_name, expected):
    # Check if number of started dockers is correct
    assert expected['dockers_num'] == len(env['docker_ids'])
    # Get the DNS ip
    dns = env['dns']
    # Will throw if the dns address is not legal
    socket.inet_aton(dns)
    # Check connectivity to node using the DNS
    # node is in form name@name.timestamp.dev.docker
    for node in env[key(component_name)]:
        (name, sep, hostname) = node.partition('@')
        ip = test_utils.dns_lookup(hostname, dns)
        assert test_utils.ping(ip)
        assert test_utils.check_http_connectivity(ip, 443, expected['test_path'], 200,
                                                  number_of_retries=50)


def up_script(name):
    return '_'.join([name, 'up.py'])


def bindir(name):
    return os.path.join(os.getcwd(), name)


def key(name):
    if name == 'globalregistry':
        name = 'gr'
    elif name == 'oneclient':
        name == 'client'
    return '_'.join([name, 'nodes'])
