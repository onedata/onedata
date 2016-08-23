"""This module contains tests of appmock mocking rest endpoints.
"""
__author__ = "Lukasz Opiola"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from environment import docker, appmock, common

from appmock import appmock_client
from tests import *
from tests.utils import net_utils
from tests.utils.path_utils import config_file, get_file_name, make_logdir


class TestAppmockRestExample:
    @classmethod
    # Run the evn_up.py script, capture and parse the output
    def setup_class(cls):
        logdir = make_logdir(ENV_UP_LOGDIR, get_file_name(__file__))
        cls.result = appmock.up(image='onedata/builder',
                                bindir=APPMOCK_DIR,
                                dns_server='none',
                                uid=common.generate_uid(),
                                config_path=os.path.join(config_file('env.json')),
                                logdir=logdir)

    @classmethod
    # Clean up removing all dockers created in the test
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # An example test showing usage of appmock in tests
    def test_rest_example(self):
        [container] = self.result['docker_ids']
        appmock_ip = docker.inspect(container)['NetworkSettings']['IPAddress'].encode(
            'ascii')
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
        # Reset counters
        appmock_client.reset_rest_history(appmock_ip)
        assert appmock_client.verify_rest_history(appmock_ip, [])
        assert 0 == appmock_client.rest_endpoint_request_count(appmock_ip, 9090, '/test_with_state')
        # And check again if they work
        some_rest_using_function(appmock_ip)
        assert appmock_client.verify_rest_history(appmock_ip, expected_history)
        assert 5 == appmock_client.rest_endpoint_request_count(appmock_ip, 9090, '/test_with_state')


# An example code which could be verified using appmock
def some_rest_using_function(appmock_ip):
    # Lets assume we are testing a code that needs to call
    # mocked component several times
    net_utils.http_get(appmock_ip, 8080, "/test1/abc")
    net_utils.http_get(appmock_ip, 8080, "/test1/abc")
    net_utils.http_get(appmock_ip, 8080, "/test2")
    net_utils.http_get(appmock_ip, 8080, "/test2")
    net_utils.http_get(appmock_ip, 8080, "/test2")
    net_utils.http_get(appmock_ip, 9090, "/test_with_state")
    net_utils.http_get(appmock_ip, 9090, "/test_with_state")
    net_utils.http_get(appmock_ip, 9090, "/test_with_state")
    net_utils.http_get(appmock_ip, 9090, "/test_with_state")
    net_utils.http_get(appmock_ip, 9090, "/test_with_state")
    net_utils.http_get(appmock_ip, 8080, "/test3")
    net_utils.http_get(appmock_ip, 443, "/some/path")
