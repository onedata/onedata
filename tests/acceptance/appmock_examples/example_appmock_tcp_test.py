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

    # An example test showing usage of appmock in tests and testing all its functionalities
    def test_tcp_example(self):
        [container] = self.result['docker_ids']
        appmock_ip = docker.inspect(container)['NetworkSettings']['IPAddress'].encode(
            'ascii')

        type1_message = 'test5\n'
        type1_message_count = 5
        type2_message = 'test12\n'
        type2_message_count = 12
        al_messages_count = type1_message_count + type2_message_count
        message_to_client = 'test_message'

        received = send_some_and_receive(appmock_ip, type1_message, type1_message_count,
                                         type2_message, type2_message_count, message_to_client)
        # Now, we can verify if expected messages were sent by the tested code
        assert type1_message_count == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type1_message)
        assert type2_message_count == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type2_message)
        assert al_messages_count == appmock_client.tcp_server_all_messages_count(appmock_ip, 5555)
        # And if 'message' was received on the socket
        assert message_to_client == received
        # Reset counters
        appmock_client.reset_tcp_server_history(appmock_ip)
        assert 0 == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type1_message)
        assert 0 == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type2_message)
        assert 0 == appmock_client.tcp_server_all_messages_count(appmock_ip, 5555)
        # And check again
        received = send_some_and_receive(appmock_ip, type1_message, type1_message_count,
                                         type2_message, type2_message_count, message_to_client)
        assert type1_message_count == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type1_message)
        assert type2_message_count == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type2_message)
        assert al_messages_count == appmock_client.tcp_server_all_messages_count(appmock_ip, 5555)
        assert message_to_client == received

        # Send multiple messages to itself and verify if they were received
        assert make_appmock_send_to_itself(appmock_ip)
        # Check counter endpoint
        wait_until_appmock_receives(appmock_ip)


# An example code which could be verified using appmock
def send_some_and_receive(appmock_ip, type1_message, type1_message_count,
                          type2_message, type2_message_count, message_to_client):
    # Lets assume we are testing a code that sends some messages on tcp port
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssl_sock = ssl.wrap_socket(s)
    ssl_sock.settimeout(10)
    ssl_sock.connect((appmock_ip, 5555))

    for i in range(type1_message_count):
        ssl_sock.send(type1_message)
    for i in range(type2_message_count):
        ssl_sock.send(type2_message)
    # TODO jesli nie ma tego sleepa to jest jakies zakleszczenie
    # nie rozumiem dlaczego. Pewnie dlatego ze z tego samego procesu
    # wysylam blokujace zadanie, zeby mi cos server przyslal.
    time.sleep(1)
    # We are running the test and tested code in the same process, so lets
    # send data from here. Normally, it would be sent from test code.
    appmock_client.tcp_server_send(appmock_ip, 5555, message_to_client, 1)
    # Now receive something and return it
    result = ssl_sock.recv()
    ssl_sock.close()
    return result


# Connects to appmock as TCP client and orders appmock to send 1000 messages to it, verifies if they came
def make_appmock_send_to_itself(appmock_ip):
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
def wait_until_appmock_receives(appmock_ip):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssl_sock = ssl.wrap_socket(s)
    ssl_sock.settimeout(10)
    ssl_sock.connect((appmock_ip, 5555))
    for i in range(1000):
        ssl_sock.send('test1\n')
        ssl_sock.send('test2\n')
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 5555, 'test1\n', 1000, False, 10)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 5555, 'test2\n', 1000, False, 10)
    appmock_client.tcp_server_wait_for_any_messages(appmock_ip, 5555, 2000, False, 10)
