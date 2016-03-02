import subprocess

from tests.test_common import *
from tests import test_utils
import socket
import time
import os
import ast
import pytest

from environment import docker, common, dns


@pytest.mark.parametrize("script_name, dockers_num", [
    # ("appmock_up", 2),
    # ("client_up", 1),
    # ("cluster_manager_up", 1),

    # TODO nie ma binarek
    # TODO ("cluster_up", {'cm_nodes': 1, 'cluster_worker_nodes': 1}) jaki path ?
    # TODO "cluster_worker_up", jaki path ?

    # ???BUG przechodzi jak nie ma sieci ("globalregistry_up", 2),
    # ???BUG: ("provider_up", {'cm_nodes': 1, 'op_worker_nodes': 2}), 2 dnsy zlaczone na liscie
    # ???BUG: ("provider_worker_up", 1), problem, bo nie wstaje worker bez cm-a

    # TODO "panel_up",
    # TODO "dns_up",
    # czy jest sens testowac skoro i tak startuje w kazdym tescie

    # brakuje cluster_name w _up
    # TODO "couchbase_up", czy jest sens stawiac bez providera ???
    # TODO "riak_up", czy jest sens stawiac bez providera ???

    # TODO ("ceph_up", 1), wiesza sie
])
def test_component_up(script_name, dockers_num):
    # setup
    environment = setup_test(script_name)
    # test
    check_if_node_is_up(environment, script_name, dockers_num)
    # teardown
    teardown_testcase(environment)


def test_s3_up():
    output = subprocess.check_output(os.path.join(docker_dir, "s3_up.py"))
    stripped_output = strip_output_logs(output)
    output_dict = ast.literal_eval(stripped_output)
    assert test_utils.ping(output_dict['host_name'].split(":")[0])


def test_dns_up():
    output = subprocess.check_output(os.path.join(docker_dir, "dns_up.py"))
    stripped_output = strip_output_logs(output)
    output_dict = ast.literal_eval(stripped_output)
    assert test_utils.ping(output_dict['dns'])



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
    print "called teardown"
    docker.remove(environment['docker_ids'], force=True, volumes=True)


# # Test if the *_up.py script works as expected.
def check_if_node_is_up(env, script_name, dockers_num):
    function_name = '_'.join(['check', script_name])
    get_function(__name__, function_name)(env, dockers_num)


def check_appmock_up(env, dockers_num):
    key = 'appmock_nodes'
    assert dockers_num == len(env[key])
    dns = env['dns']
    # Will throw if the dns address is not legal
    socket.inet_aton(dns)
    # Check connectivity to nodes using the DNS
    # node is in form name@name.timestamp.dev.docker
    for node in env[key]:
        (name, sep, hostname) = node.partition('@')
        ip = test_utils.dns_lookup(hostname, dns)
        assert test_utils.ping(ip)
        assert test_utils.check_http_connectivity(ip, 443, '/test2', 200,
                                                  number_of_retries=50)


def check_client_up(env, dockers_num):
    assert dockers_num == len(env['docker_ids']) - 1
    dns = env['dns']
    # Will throw if the dns address is not legal
    socket.inet_aton(dns)
    # Check client nodes
    # oc_node is in form name.timestamp.dev.docker
    for oc_node in env['client_nodes']:
        oc_ip = test_utils.dns_lookup(oc_node, dns)
        assert test_utils.ping(oc_ip)


def check_cluster_manager_up(env, dockers_num):
    key = 'cluster_manager_nodes'
    assert dockers_num == len(env[key])
    dns = env['dns']
    # Will throw if the dns address is not legal
    socket.inet_aton(dns)
    # Check connectivity to nodes using the DNS
    # Check OP CM nodes
    # cm_node is in form name@name.timestamp.dev.docker
    for cm_node in env[key]:
        (cm_name, sep, cm_hostname) = cm_node.partition('@')
        cm_ip = test_utils.dns_lookup(cm_hostname, dns)
        assert test_utils.ping(cm_ip)


def check_cluster_up(env, dockers_num):
    check_cluster_manager_up(env, dockers_num['cm_nodes'])


def check_globalregistry_up(env, dockers_num):
    key = 'cluster_manager_nodes'
    assert dockers_num == len(env[key]) - 1
    dns = env['dns']
    # Will throw if the dns address is not legal
    socket.inet_aton(dns)
    # Check connectivity to nodes using the DNS
    # Check GR nodes
    # gr_node is in form name@name.timestamp.dev.docker
    for gr_node in env[key]:
        (gr_name, sep, gr_hostname) = gr_node.partition('@')
        gr_ip = test_utils.dns_lookup(gr_hostname, dns)
        assert test_utils.ping(gr_ip)
        assert test_utils.check_http_connectivity(gr_ip, 443, '/', 200,
                                                  number_of_retries=50)

    # Check GR DB nodes
    # gr_db_node is in form name@name.timestamp.dev.docker
    for gr_db_node in env['gr_db_nodes']:
        (gr_db_name, sep, gr_db_hostname) = gr_db_node.partition('@')
        gr_db_ip = test_utils.dns_lookup(gr_db_hostname, dns)
        assert test_utils.ping(gr_db_ip)
        assert test_utils.check_http_connectivity(gr_db_ip, 5984, '/_utils/',
                                                  200, use_ssl=False,
                                                  number_of_retries=50)


def check_provider_up(env, dockers_num):
    check_cluster_manager_up(env, dockers_num['cm_nodes'])
    check_provider_worker_up(env, dockers_num['op_worker'])


def check_provider_worker_up(env, dockers_num):
    key = 'op_worker_nodes'
    assert dockers_num * 2 == len(env[key])
    dns = env['dns']
    # Will throw if the dns address is not legal
    socket.inet_aton(dns)
    for w_node in env[key]:
        (w_name, sep, w_hostname) = w_node.partition('@')
        w_ip = test_utils.dns_lookup(w_hostname, dns)
        assert test_utils.ping(w_ip)
        assert test_utils.check_http_connectivity(w_ip, 6666, '/nagios', 200,
                                                  use_ssl=False,
                                                  number_of_retries=50)


#########################################################################
# UTILS


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
            '-bw', os.path.join(os.getcwd(), os.path.join('op_worker',
                                                          'cluster_worker')),
            #TODO nie wiem jaki path
            #TODO cluster_up nie ma domains_name
            '-bcm', os.path.join(os.getcwd(), 'cluster_manager')
        ]
    elif name == "client":
        name = "oneclient"
    elif name == "panel":
        name = "onepanel"
    return ['-b', os.path.join(os.getcwd(), name)]


def get_module(name):
    return sys.modules[name]


def get_function(module_name, function_name):
    return getattr(get_module(module_name), function_name)


def strip_output_logs(output):
    return output.strip().split('\n')[-1]


def prepare_cmd(script_name, uid, dns_server, config_path):
    cmd = [os.path.join(docker_dir, up_script(script_name))]
    cmd.extend(bindir_options(script_name)),
    cmd.extend([
        '-d', dns_server,
        '-u', uid
    ])

    # TODO delete if after resolving VFS-1641
    if script_name != "client_up":
        cmd.extend([
            '-l', get_logdir_name(acceptance_logdir, get_test_name(__file__))
        ])
    cmd.append(config_path)

    return cmd