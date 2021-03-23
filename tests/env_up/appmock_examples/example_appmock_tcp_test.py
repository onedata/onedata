"""This module contains tests of appmock mocking tcp server.
"""
__author__ = "Lukasz Opiola"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from tests import *
from tests.utils.path_utils import config_file, get_file_name, make_logdir

from environment import docker, appmock, common
from appmock import appmock_client

import socket
import ssl
import time


class TestAppmockTCPExample:
    @classmethod
    # Run the evn_up.py script, capture and parse the output
    def setup_class(cls):
        logdir = make_logdir(ENV_UP_LOGDIR, get_file_name(__file__))
        cls.result = appmock.up(image='onedata/builder', bindir=APPMOCK_DIR,
                                dns_server='none', uid=common.generate_uid(),
                                config_path=os.path.join(config_file('env.json')),
                                logdir=logdir)

    @classmethod
    # Clean up removing all dockers created in the test
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # An example test showing usage of appmock in tests and testing all its functionalities
    def test_tcp_example(self):
        [container] = self.result['docker_ids']
        appmock_ip = docker.inspect(container)['NetworkSettings']['IPAddress'].encode(
                'ascii')
        # Send some requests, check counters, order appmock to send something to the client and receive it
        send_some_and_receive(appmock_ip)
        # Send multiple messages to itself and verify if they were received
        make_appmock_send_to_itself(appmock_ip)
        # Check waiting for messages
        wait_until_appmock_receives(appmock_ip)
        # Check counter endpoint
        use_counter_endpoint(appmock_ip)


# An example code which could be verified using appmock
def send_some_and_receive(appmock_ip):
    # Lets assume we are testing a code that sends some messages on tcp port
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssl_sock = ssl.wrap_socket(s)
    ssl_sock.settimeout(20)
    ssl_sock.connect((appmock_ip, 5555))

    type1_message = 'test5\n'
    type1_message_count = 5
    type2_message = 'test12\n'
    type2_message_count = 12
    all_messages_count = type1_message_count + type2_message_count
    message_to_client = 'test_message'

    def send_some():
        for i in range(type1_message_count):
            ssl_sock.send(type1_message)
        for i in range(type2_message_count):
            ssl_sock.send(type2_message)
        # NOTE: without the sleep, a deadlock can occur, but it's unclear why
        time.sleep(1)
        # We are running the test and tested code in the same process, so lets
        # send data from here. Normally, it would be sent from test code.
        appmock_client.tcp_server_send(appmock_ip, 5555, message_to_client, 1)
        # Now receive something and return it
        result = ssl_sock.recv()
        return result

    received = send_some()
    # Now, we can verify if expected messages were sent by the tested code
    assert type1_message_count == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type1_message)
    assert type2_message_count == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type2_message)
    assert all_messages_count == appmock_client.tcp_server_all_messages_count(appmock_ip, 5555)
    # And if 'message' was received on the socket
    assert message_to_client == received
    # Reset counters
    appmock_client.reset_tcp_server_history(appmock_ip)
    assert 0 == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type1_message)
    assert 0 == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type2_message)
    assert 0 == appmock_client.tcp_server_all_messages_count(appmock_ip, 5555)
    # And check again
    received = send_some()
    assert type1_message_count == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type1_message)
    assert type2_message_count == appmock_client.tcp_server_specific_message_count(appmock_ip, 5555, type2_message)
    assert all_messages_count == appmock_client.tcp_server_all_messages_count(appmock_ip, 5555)
    assert message_to_client == received
    ssl_sock.close()


# Connects to appmock as TCP client and orders appmock to send 1000 messages to it, verifies if they came
def make_appmock_send_to_itself(appmock_ip):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssl_sock = ssl.wrap_socket(s)
    ssl_sock.settimeout(20)
    ssl_sock.connect((appmock_ip, 5555))
    appmock_client.tcp_server_send(appmock_ip, 5555, 'message', 1000)
    # Now receive 10 messages
    result = True
    for i in range(1000):
        result = result and 'message' == ssl_sock.recv()
    ssl_sock.close()
    assert result == True


# Connects to appmock as TCP client and sends 1000 requests, waits for them to be received.
def wait_until_appmock_receives(appmock_ip):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssl_sock = ssl.wrap_socket(s)
    ssl_sock.settimeout(20)
    ssl_sock.connect((appmock_ip, 5555))
    for i in range(1000):
        ssl_sock.send('test1\n')
        ssl_sock.send('test2\n')
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 5555, 'test1\n', 1000)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 5555, 'test2\n', 1000)
    ssl_sock.send('test1\n')
    ssl_sock.send('test2\n')
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 5555, 'test1\n', 1000, accept_more=True)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 5555, 'test2\n', 1000, accept_more=True)
    appmock_client.reset_tcp_server_history(appmock_ip)
    for i in range(1000):
        ssl_sock.send('sdfg\n')
        ssl_sock.send('adfgsdf\n')
        ssl_sock.send('345rthas\n')
        ssl_sock.send('367sth\n')
        ssl_sock.send('qatert547\n')
    appmock_client.tcp_server_wait_for_any_messages(appmock_ip, 5555, 5000)
    appmock_client.reset_tcp_server_history(appmock_ip)
    for i in range(5):
        ssl_sock.send('a\n')
    for i in range(5):
        ssl_sock.send('b\n')
    correct_history = ['a\n', 'a\n', 'a\n', 'a\n', 'a\n', 'b\n', 'b\n', 'b\n', 'b\n', 'b\n']
    # Wait for messages to arrive
    appmock_client.tcp_server_wait_for_any_messages(appmock_ip, 5555, 10)
    # Test the correctness of tcp_server_history endpoint
    result = appmock_client.tcp_server_history(appmock_ip, 5555)
    assert result == correct_history
    # Test if wait functions correctly returns msg history if requested.
    result = appmock_client.tcp_server_wait_for_any_messages(appmock_ip, 5555, 10, return_history=True)
    assert result == correct_history
    result = appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 5555, 'a\n', 5, return_history=True)
    assert result == correct_history
    result = appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 5555, 'b\n', 5, return_history=True)
    assert result == correct_history


# Connects to appmock as TCP client and sends 100000 requests on counter endpoint, waits for them to be received.
def use_counter_endpoint(appmock_ip):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    ssl_sock = ssl.wrap_socket(s)
    ssl_sock.settimeout(20)
    ssl_sock.connect((appmock_ip, 6666))
    for i in range(20000):
        ssl_sock.send('sdfg\n')
        ssl_sock.send('adfgsdf\n')
        ssl_sock.send('345rthas\n')
        ssl_sock.send('367sth\n')
        ssl_sock.send('qatert547\n')
    appmock_client.tcp_server_wait_for_any_messages(appmock_ip, 6666, 100000)
    assert 100000 == appmock_client.tcp_server_all_messages_count(appmock_ip, 6666)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 6666, 'sdfg\n', 20000)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 6666, 'adfgsdf\n', 20000)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 6666, '345rthas\n', 20000)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 6666, '367sth\n', 20000)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 6666, 'qatert547\n', 20000)
    assert 20000 == appmock_client.tcp_server_specific_message_count(appmock_ip, 6666, 'sdfg\n')
    assert 20000 == appmock_client.tcp_server_specific_message_count(appmock_ip, 6666, 'adfgsdf\n')
    assert 20000 == appmock_client.tcp_server_specific_message_count(appmock_ip, 6666, '345rthas\n')
    assert 20000 == appmock_client.tcp_server_specific_message_count(appmock_ip, 6666, '367sth\n')
    assert 20000 == appmock_client.tcp_server_specific_message_count(appmock_ip, 6666, 'qatert547\n')
    appmock_client.reset_tcp_server_history(appmock_ip)
    for i in range(2000):
        ssl_sock.send('sdfg\n')
        ssl_sock.send('adfgsdf\n')
        ssl_sock.send('345rthas\n')
        ssl_sock.send('367sth\n')
        ssl_sock.send('qatert547\n')
    appmock_client.tcp_server_wait_for_any_messages(appmock_ip, 6666, 10000)
    assert 10000 == appmock_client.tcp_server_all_messages_count(appmock_ip, 6666)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 6666, 'sdfg\n', 2000)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 6666, 'adfgsdf\n', 2000)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 6666, '345rthas\n', 2000)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 6666, '367sth\n', 2000)
    appmock_client.tcp_server_wait_for_specific_messages(appmock_ip, 6666, 'qatert547\n', 2000)
    assert 2000 == appmock_client.tcp_server_specific_message_count(appmock_ip, 6666, 'sdfg\n')
    assert 2000 == appmock_client.tcp_server_specific_message_count(appmock_ip, 6666, 'adfgsdf\n')
    assert 2000 == appmock_client.tcp_server_specific_message_count(appmock_ip, 6666, '345rthas\n')
    assert 2000 == appmock_client.tcp_server_specific_message_count(appmock_ip, 6666, '367sth\n')
    assert 2000 == appmock_client.tcp_server_specific_message_count(appmock_ip, 6666, 'qatert547\n')
    appmock_client.reset_tcp_server_history(appmock_ip)
    appmock_client.tcp_server_wait_for_any_messages(appmock_ip, 6666, 0)
