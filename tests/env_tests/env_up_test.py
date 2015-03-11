from tests import testutil
import json
import socket
import time


# Helper function to check https connectivity
# Retries once a second, up to given number of times.
def check_http_connectivity(ip, port, path, expected_code, use_ssl=True, number_of_retries=20):
    if number_of_retries == 0:
        protocol = 'https' if use_ssl else 'http'
        raise Exception('{0}://{1}:{2}{3} is unreachable.'.format(protocol, ip, port, path))
    else:
        try:
            status_code, _, _ = testutil.http_get(ip, port, path, use_ssl)
            return expected_code == status_code
        except:
            time.sleep(1)
            return check_http_connectivity(ip, port, path, expected_code, use_ssl, number_of_retries - 1)


class TestEnvUp:
    @classmethod
    # Run the evn_up.py script, capture and parse the output
    def setup_class(cls):
        cmd_result = testutil.run_command(['bamboos/docker/env_up.py', testutil.test_file('env.json')])
        cls.result = json.loads(cmd_result)

    @classmethod
    # Clean up removing all dockers created in the test
    def teardown_class(cls):
        for docker_id in cls.result['docker_ids']:
            testutil.run_command(['docker', 'kill', docker_id])
            testutil.run_command(['docker', 'rm', docker_id])

    # Test if the env_up.py script works as expected.
    def test_env_up(self):
        # For shorter code
        res = self.result
        # Check if number of started dockers is correct
        # The number should be:
        # 2 for skydns and skydock
        # 3 OP nodes
        # 1 Riak node for OP
        # 2 GR nodes
        # 2 DBs for GR nodes
        # 2 appmock nodes
        # 2 client nodes
        # ------------
        # 14 nodes
        assert 14 == len(res['docker_ids'])
        # Get the DNS ip
        dns = res['dns']
        # Will throw if the dns address is not legal
        socket.inet_aton(dns)
        # Check connectivity to system components using the DNS
        # Check GR nodes
        # gr_node is in form name@name.timestamp.dev.docker
        for gr_node in res['gr_nodes']:
            (gr_name, sep, gr_hostname) = gr_node.partition('@')
            gr_ip = testutil.dns_lookup(gr_hostname, dns)
            assert testutil.ping(gr_ip)
            assert check_http_connectivity(gr_ip, 443, '/', 200, number_of_retries=50)

        # Check GR DB nodes - they are not visible in dns, so we must check
        # their ips through docker.
        # gr_db_node is in form name@name.timestamp.dev.docker
        for gr_db_node in res['gr_db_nodes']:
            (gr_db_name, sep, gr_db_hostname) = gr_db_node.partition('@')
            docker_name = gr_db_hostname.rstrip('.dev.docker').replace('.', '_')
            gr_db_ip = testutil.run_command(['docker inspect -f "{{ .NetworkSettings.IPAddress }}"', docker_name])
            assert testutil.ping(gr_db_ip)
            assert check_http_connectivity(gr_db_ip, 5984, '/_utils/', 200, use_ssl=False, number_of_retries=50)

        # Check OP CCM nodes
        # ccm_node is in form name@name.timestamp.dev.docker
        for ccm_node in res['op_ccm_nodes']:
            (ccm_name, sep, ccm_hostname) = ccm_node.partition('@')
            ccm_ip = testutil.dns_lookup(ccm_hostname, dns)
            assert testutil.ping(ccm_ip)

        # Check OP worker nodes
        # w_node is in form name@name.timestamp.dev.docker
        for w_node in res['op_worker_nodes']:
            (w_name, sep, w_hostname) = w_node.partition('@')
            w_ip = testutil.dns_lookup(w_hostname, dns)
            assert testutil.ping(w_ip)
            assert check_http_connectivity(w_ip, 443, '/', 200, number_of_retries=50)

        # Check appmock nodes
        # am_node is in form name@name.timestamp.dev.docker
        for am_node in res['appmock_nodes']:
            (am_name, sep, am_hostname) = am_node.partition('@')
            am_ip = testutil.dns_lookup(am_hostname, dns)
            assert testutil.ping(am_ip)
            assert check_http_connectivity(am_ip, 443, '/test2', 200, number_of_retries=50)

        # Check client nodes - they are not visible in dns, so we must check
        # their ips through docker.
        # oc_node is in form name.timestamp.dev.docker
        for oc_node in res['client_nodes']:
            docker_name = oc_node.rstrip('.dev.docker').replace('.', '_')
            oc_ip = testutil.run_command(['docker inspect -f "{{ .NetworkSettings.IPAddress }}"', docker_name])
            assert testutil.ping(oc_ip)

