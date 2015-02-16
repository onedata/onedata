import json
from tests import testutil
import socket
import requests
import time


# Helper function to check host's ip using given dns
def dns_lookup(dns, host):
    return testutil.run_command(['dig', '+short', '@' + dns, host])


# Helper function that checks if given ip is reachable with ping
def ping(ip):
    return '0' == testutil.run_command(['ping', '-c 1', ip, '1>/dev/null;', 'echo $?'])


# Helper function to check https connectivity
# Returned value means if expected status code was the same as actual.
def http_get(ip, port, path, expected_code, use_ssl):
    protocol = 'https' if use_ssl else 'http'
    response = requests.get('{0}://{1}:{2}{3}'.format(protocol, ip, port, path), verify=False, timeout=0.3)
    return expected_code == response.status_code


# Helper function to check https connectivity
# Retries once a second, up to given number of times.
def http_get_retry(ip, port, path, expected_code, use_ssl=True, number_of_retries=20):
    if number_of_retries == 0:
        protocol = 'https' if use_ssl else 'http'
        raise Exception('{0}://{1}:{2}{3} is unreachable.'.format(protocol, ip, port, path))
    else:
        try:
            return http_get(ip, port, path, expected_code, use_ssl)
        except:
            time.sleep(1)
            return http_get_retry(ip, port, path, expected_code, use_ssl, number_of_retries - 1)



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

    def test_my(self):
        assert True

    # Test if the env_up.py script works as expected.
    def test_env_up(self):
        # For shorter code
        res = self.result
        # Check if number of started dockers is correct
        # The number should be:
        # 2 for skydns and skydock
        # 3 OP nodes
        # 2 GR nodes
        # 2 DBs for GR nodes
        # 2 appmock nodes
        # 2 client nodes
        # ------------
        # 13 nodes
        assert 13 == len(res['docker_ids'])
        # Get the DNS ip
        dns = res['dns']
        # Will throw if the dns address is not legal
        socket.inet_aton(dns)
        # Check connectivity to system components using the DNS
        # Check GR nodes
        # gr_node is in form name@name.timestamp.dev.docker
        for gr_node in res['gr_nodes']:
            (gr_name, sep, gr_hostname) = gr_node.partition('@')
            gr_ip = dns_lookup(dns, gr_hostname)
            assert ping(gr_ip)
            assert http_get_retry(gr_ip, 443, '/', 200, number_of_retries=50)

        # Check GR DB nodes - they are not visible in dns, so we must check
        # their ips through docker.
        # gr_db_node is in form name@name.timestamp.dev.docker
        for gr_db_node in res['gr_db_nodes']:
            (gr_db_name, sep, gr_db_hostname) = gr_db_node.partition('@')
            dockername = gr_db_hostname.rstrip('.dev.docker').replace('.', '_')
            gr_db_ip = testutil.run_command(['docker inspect -f "{{ .NetworkSettings.IPAddress }}"', dockername])
            assert ping(gr_db_ip)
            assert http_get_retry(gr_db_ip, 5984, '/_utils/', 200, use_ssl=False, number_of_retries=50)

        # Check OP CCM nodes
        # ccm_node is in form name@name.timestamp.dev.docker
        for ccm_node in res['op_ccm_nodes']:
            (ccm_name, sep, ccm_hostname) = ccm_node.partition('@')
            ccm_ip = dns_lookup(dns, ccm_hostname)
            assert ping(ccm_ip)

        # Check OP worker nodes
        # w_node is in form name@name.timestamp.dev.docker
        for w_node in res['op_worker_nodes']:
            (w_name, sep, w_hostname) = w_node.partition('@')
            w_ip = dns_lookup(dns, w_hostname)
            assert ping(w_ip)
            assert http_get_retry(w_ip, 443, '/', 200, number_of_retries=50)

        # Check appmock nodes
        # am_node is in form name@name.timestamp.dev.docker
        for am_node in res['appmock_nodes']:
            (am_name, sep, am_hostname) = am_node.partition('@')
            am_ip = dns_lookup(dns, am_hostname)
            assert ping(am_ip)
            assert http_get_retry(am_ip, 443, '/test2', 200, number_of_retries=50)

        # Check client nodes - they are not visible in dns, so we must check
        # their ips through docker.
        # oc_node is in form name.timestamp.dev.docker
        for oc_node in res['client_nodes']:
            dockername = oc_node.rstrip('.dev.docker').replace('.', '_')
            oc_ip = testutil.run_command(['docker inspect -f "{{ .NetworkSettings.IPAddress }}"', dockername])
            assert ping(oc_ip)


