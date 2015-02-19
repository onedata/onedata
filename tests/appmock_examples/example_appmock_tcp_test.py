from tests import testutil
from tests import appmock_client
import json
import time
import ssl
import socket


class TestAppmockTCPExample:
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

    # An example test showing usage of appmock in tests
    def test_tcp_example(self):
        res = self.result
        dns_addr = res['dns']
        docker_name = res['appmock_nodes'][0]
        (_, _, docker_hostname) = docker_name.partition('@')
        appmock_ip = testutil.dns_lookup(docker_hostname, dns_addr)
        # TODO remove this sleep when appmock start is verified with nagios
        time.sleep(3)
        # Run the tested code
        received = some_tcp_using_function(appmock_ip)
        # Now, we can verify if expected messages were sent by the tested code
        assert 3 == appmock_client.tcp_server_message_count(appmock_ip, 5555, 'test\n')
        # And if "message" was received on the socket
        assert "message" == received


# An example code which could be verified using appmock
def some_tcp_using_function(appmock_ip):
    # Lets assume we are testing a code that sends some messages on tcp port
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssl_sock = ssl.wrap_socket(s)
    ssl_sock.connect((appmock_ip, 5555))
    ssl_sock.send("test\n")
    ssl_sock.send("test\n")
    ssl_sock.send("test\n")
    # TODO jesli nie ma tego sleepa to jest jakies zakleszczenie
    # nie rozumiem dlaczego. Pewnie dlatego ze z tego samego procesu
    # wysylam blokujace zadanie, zeby mi cos server przyslal.
    time.sleep(1)
    # We are running the test and tested code in the same process, so lets
    # send data from here. Normally, it would be sent from test code.
    appmock_client.tcp_server_send(appmock_ip, 5555, 'message')
    # Now receive something and return it
    result = ssl_sock.recv()
    ssl_sock.close()
    return result