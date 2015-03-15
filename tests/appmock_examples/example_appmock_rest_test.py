from tests import testutil
import json
import time
import sys
import os

appmock_dir = os.path.join(os.getcwd(), 'appmock')
sys.path.insert(0, appmock_dir)
bamboos_dir = os.path.join(os.getcwd(), 'bamboos', 'docker')
sys.path.insert(0, bamboos_dir)
from appmock import appmock_client
from environment import docker


class TestAppmockRestExample:
    @classmethod
    # Run the evn_up.py script, capture and parse the output
    def setup_class(cls):
        cmd_result = testutil.run_command(['bamboos/docker/env_up.py', testutil.test_file('env.json')])
        cls.result = json.loads(cmd_result)

    @classmethod
    # Clean up removing all dockers created in the test
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # An example test showing usage of appmock in tests
    def test_rest_example(self):
        res = self.result
        dns_addr = res['dns']
        docker_name = res['appmock_nodes'][0]
        (_, _, docker_hostname) = docker_name.partition('@')
        appmock_ip = testutil.dns_lookup(docker_hostname, dns_addr)
        # TODO remove this sleep when appmock start is verified with nagios
        time.sleep(3)
        # Run the tested code
        some_rest_using_function(appmock_ip)
        # Now, we can verify if expected requests were made by the tested code
        expected_history = [
            (8080, '/test1/[:binding]'),
            (8080, '/test1/[:binding]'),
            (8080, '/test2'),
            (8080, '/test2'),
            (8080, '/test2'),
            (9090, '/test_with_state'),
            (9090, '/test_with_state'),
            (9090, '/test_with_state'),
            (9090, '/test_with_state'),
            (9090, '/test_with_state'),
            (443, '/[:binding/[...]]')
        ]
        assert appmock_client.verify_rest_history(appmock_ip, expected_history)
        # Get number of requests on certain endpoint and check if it matches the expected value.
        assert 5 == appmock_client.rest_endpoint_request_count(appmock_ip, 9090, '/test_with_state')


# An example code which could be verified using appmock
def some_rest_using_function(appmock_ip):
    # Lets assume we are testing a code that needs to call
    # mocked component several times
    testutil.http_get(appmock_ip, 8080, "/test1/abc", True)
    testutil.http_get(appmock_ip, 8080, "/test1/abc", True)
    testutil.http_get(appmock_ip, 8080, "/test2", True)
    testutil.http_get(appmock_ip, 8080, "/test2", True)
    testutil.http_get(appmock_ip, 8080, "/test2", True)
    testutil.http_get(appmock_ip, 9090, "/test_with_state", True)
    testutil.http_get(appmock_ip, 9090, "/test_with_state", True)
    testutil.http_get(appmock_ip, 9090, "/test_with_state", True)
    testutil.http_get(appmock_ip, 9090, "/test_with_state", True)
    testutil.http_get(appmock_ip, 9090, "/test_with_state", True)
    testutil.http_get(appmock_ip, 8080, "/test3", True)
    testutil.http_get(appmock_ip, 443, "/some/path", True)
