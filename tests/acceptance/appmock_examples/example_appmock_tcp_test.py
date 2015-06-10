from tests import testutil
import json
import time
import ssl
import socket
import sys
import os

appmock_dir = os.path.join(os.getcwd(), 'appmock')
sys.path.insert(0, appmock_dir)
bamboos_dir = os.path.join(os.getcwd(), 'bamboos', 'docker')
sys.path.insert(0, bamboos_dir)
from appmock import appmock_client
from environment import docker
from environment import appmock
from environment import common


class TestAppmockTCPExample:
    @classmethod
    # Run the evn_up.py script, capture and parse the output
    def setup_class(cls):
        cls.result = appmock.up(image='onedata/builder', bindir=appmock_dir,
                                dns='none', uid=common.generate_uid(),
                                config_path=os.path.join(testutil.test_file('env.json')))

    @classmethod
    # Clean up removing all dockers created in the test
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # An example test showing usage of appmock in tests
    def test_tcp_example(self):
        [container] = self.result['docker_ids']
        appmock_ip = docker.inspect(container)['NetworkSettings']['IPAddress'].encode(
            'ascii')
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