import subprocess

from tests.test_common import *
from tests import test_utils
import socket
import time
import os
import ast

from environment import docker, env, common, dns, appmock

class TestAppmockUp:
    @classmethod
    # Run the evn_up.py script, capture and parse the output
    def setup_class(cls):
        logdir = get_logdir_name(acceptance_logdir, get_test_name(__file__))
        config_path = test_utils.test_file('env.json')
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
            os.path.join(docker_dir, "appmock_up.py"),
            '-i', env.default('image'),
            '-b', env.default('bin_am'),
            '-d', dns_server,
            '-u', uid,
            '-l', logdir,
            config_path
        ])

        am_output = ast.literal_eval(am_output)
        common.merge(output, am_output)
        cls.result = output

    @classmethod
    # Clean up removing all dockers created in the test
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # Test if the appmock_up.py script works as expected.
    def test_appmock_up(self):
        res = self.result
        # Check if number of started dockers is correct
        # The number should be: 2
        assert 3 == len(res['docker_ids'])
        # Get the DNS ip
        dns = res['dns']
        # Will throw if the dns address is not legal
        socket.inet_aton(dns)
        # Check connectivity to appmock node using the DNS

        # Check appmock nodes
        # am_node is in form name@name.timestamp.dev.docker
        for am_node in res['appmock_nodes']:
            (am_name, sep, am_hostname) = am_node.partition('@')
            am_ip = test_utils.dns_lookup(am_hostname, dns)
            assert test_utils.ping(am_ip)
            assert test_utils.check_http_connectivity(am_ip, 443, '/test2', 200,
                                                      number_of_retries=50)
