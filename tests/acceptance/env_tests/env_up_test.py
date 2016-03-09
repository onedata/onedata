from tests.test_common import *
from tests import test_utils
import socket
import time
import os

from environment import docker, env


class TestEnvUp:
    @classmethod
    # Run the evn_up.py script, capture and parse the output
    def setup_class(cls):
        logdir = make_logdir(acceptance_logdir, get_test_name(__file__))
        cls.result = env.up(test_utils.test_file('env.json'), logdir=logdir)

    @classmethod
    # Clean up removing all dockers created in the test
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # Test if the env_up.py script works as expected.
    def test_env_up(self):
        res = self.result
        print(res)

        # Check if number of started dockers is correct
        # The number should be:
        # 1 dns node

        # 4 op_worker nodes
        # 2 cluster_manager nodes for op_workers
        # 2 DB nodes for op_workers

        # 1 OZ nodes
        # 1 cluster_manager nodes for oz_worker
        # 1 DB node for oz_worker

        # 2 appmock nodes

        # 2 client nodes
        # ------------
        # 16 nodes
        assert 16 == len(res['docker_ids'])
        # Get the DNS ip
        dns = res['dns']
        # Will throw if the dns address is not legal
        socket.inet_aton(dns)
        # Check connectivity to system components using the DNS
        # Check OZ nodes
        # oz_node is in form name@name.timestamp.dev.docker
        for oz_node in res['oz_worker_nodes']:
            (oz_name, sep, oz_hostname) = oz_node.partition('@')
            oz_ip = test_utils.dns_lookup(oz_hostname, dns)
            assert test_utils.ping(oz_ip)
            assert check_http_connectivity(oz_ip, 443, '/', 200, number_of_retries=50)

        # Check OP CM nodes
        # cm_node is in form name@name.timestamp.dev.docker
        for cm_node in res['cluster_manager_nodes']:
            (cm_name, sep, cm_hostname) = cm_node.partition('@')
            cm_ip = test_utils.dns_lookup(cm_hostname, dns)
            assert test_utils.ping(cm_ip)

        # Check OP worker nodes
        # w_node is in form name@name.timestamp.dev.docker
        for w_node in res['op_worker_nodes']:
            (w_name, sep, w_hostname) = w_node.partition('@')
            w_ip = test_utils.dns_lookup(w_hostname, dns)
            assert test_utils.ping(w_ip)
            assert check_http_connectivity(w_ip, 6666, '/nagios', 200, use_ssl=False, number_of_retries=50)

        # Check appmock nodes
        # am_node is in form name@name.timestamp.dev.docker
        for am_node in res['appmock_nodes']:
            (am_name, sep, am_hostname) = am_node.partition('@')
            am_ip = test_utils.dns_lookup(am_hostname, dns)
            assert test_utils.ping(am_ip)
            assert check_http_connectivity(am_ip, 443, '/test2', 200, number_of_retries=50)

        # Check client nodes
        # oc_node is in form name.timestamp.dev.docker
        for oc_node in res['client_nodes']:
            oc_ip = test_utils.dns_lookup(oc_node, dns)
            assert test_utils.ping(oc_ip)


# Helper function to check https connectivity
# Retries once a second, up to given number of times.
def check_http_connectivity(ip, port, path, expected_code, use_ssl=True, number_of_retries=20):
    if number_of_retries == 0:
        protocol = 'https' if use_ssl else 'http'
        raise Exception('{0}://{1}:{2}{3} is unreachable.'.format(protocol, ip, port, path))
    else:
        try:
            status_code, _, _ = test_utils.http_get(ip, port, path, use_ssl)
            return expected_code == status_code
        except:
            time.sleep(1)
            return check_http_connectivity(ip, port, path, expected_code, use_ssl, number_of_retries - 1)

