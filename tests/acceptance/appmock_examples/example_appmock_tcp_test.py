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
        assert 3 == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, 'test3\n')
        assert 4 == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, 'test4\n')
        assert 7 == appmock_client.tcp_server_all_messages_count(appmock_ip, 5555)
        # And if "message" was received on the socket
        assert "message" == received
        # Reset counters
        appmock_client.reset_tcp_server_history(appmock_ip)
        assert 0 == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, 'test3\n')
        assert 0 == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, 'test4\n')
        assert 0 == appmock_client.tcp_server_all_messages_count(appmock_ip, 5555)
        # And check again
        received = some_tcp_using_function(appmock_ip)
        assert 3 == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, 'test3\n')
        assert 4 == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, 'test4\n')
        assert 7 == appmock_client.tcp_server_all_messages_count(appmock_ip, 5555)
        assert "message" == received
        # Send multiple messages to itself and verify if they were received
        assert send_1000_to_itself(appmock_ip)
        # Check counter endpoint
        send_1000_and_wait(appmock_ip)



# An example code which could be verified using appmock
def some_tcp_using_function(appmock_ip):
    # Lets assume we are testing a code that sends some messages on tcp port
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssl_sock = ssl.wrap_socket(s)
    ssl_sock.settimeout(10)
    ssl_sock.connect((appmock_ip, 5555))
    ssl_sock.send('test3\n')
    ssl_sock.send('test3\n')
    ssl_sock.send('test3\n')
    ssl_sock.send('test4\n')
    ssl_sock.send('test4\n')
    ssl_sock.send('test4\n')
    ssl_sock.send('test4\n')
    # TODO jesli nie ma tego sleepa to jest jakies zakleszczenie
    # nie rozumiem dlaczego. Pewnie dlatego ze z tego samego procesu
    # wysylam blokujace zadanie, zeby mi cos server przyslal.
    time.sleep(1)
    # We are running the test and tested code in the same process, so lets
    # send data from here. Normally, it would be sent from test code.
    appmock_client.tcp_server_send(appmock_ip, 5555, 'message', 1)
    # Now receive something and return it
    result = ssl_sock.recv()
    ssl_sock.close()
    return result


# Connects to appmock as TCP client and orders appmock to send 1000 messages to it, verifies if they came
def send_1000_to_itself(appmock_ip):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssl_sock = ssl.wrap_socket(s)
    ssl_sock.settimeout(10)
    ssl_sock.connect((appmock_ip, 5555))
    appmock_client.tcp_server_send(appmock_ip, 5555, 'message', 1000)
    # Now receive 10 messages
    result = True
    for i in range(1000):
        result = result and 'message' == ssl_sock.recv()
    ssl_sock.close()
    return result


# Connects to appmock as TCP client and sends 1000 requests, waits for them to be received.
def send_1000_and_wait(appmock_ip):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssl_sock = ssl.wrap_socket(s)
    ssl_sock.settimeout(10)
    ssl_sock.connect((appmock_ip, 5555))
    for i in range(1000):
        ssl_sock.send('test1\n')
        ssl_sock.send('test2\n')
    appmock_client.tcp_server_wait_for_messages(appmock_ip, 5555, 'test1\n', 1000, False, 10)
    appmock_client.tcp_server_wait_for_messages(appmock_ip, 5555, 'test2\n', 1000, False, 10)

