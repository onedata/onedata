from tests import testutil
import json
import time


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
            pass
            # testutil.run_command(['docker', 'kill', docker_id])
            # testutil.run_command(['docker', 'rm', docker_id])

    # An example test showing usage of appmock in tests
    def test_example(self):
        res = self.result
        dns_addr = res['dns']
        docker_name = res['appmock_nodes'][0]
        (_, _, docker_hostname) = docker_name.partition('@')
        docker_ip = testutil.dns_lookup(docker_hostname, dns_addr)
        # TODO remove this sleep when appmock start is verified with nagios
        time.sleep(5)
        some_tested_function(docker_ip)
        # Now, we can verify if expected requests were made by the tested code



# An example code which could be verified using appmock
def some_tested_function(docker_ip):
    # Lets assume we are testing a code that needs to call
    # mocked component several times
    testutil.http_get(docker_ip, 8080, "/test1/abc", True)
    testutil.http_get(docker_ip, 8080, "/test1/abc", True)
    testutil.http_get(docker_ip, 8080, "/test2", True)
    testutil.http_get(docker_ip, 8080, "/test2", True)
    testutil.http_get(docker_ip, 8080, "/test2", True)
    testutil.http_get(docker_ip, 9090, "/test_with_state", True)
    testutil.http_get(docker_ip, 9090, "/test_with_state", True)
    testutil.http_get(docker_ip, 9090, "/test_with_state", True)
    testutil.http_get(docker_ip, 9090, "/test_with_state", True)
    testutil.http_get(docker_ip, 9090, "/test_with_state", True)
    testutil.http_get(docker_ip, 8080, "/test3", True)
    testutil.http_get(docker_ip, 443,  "/some/path", True)
