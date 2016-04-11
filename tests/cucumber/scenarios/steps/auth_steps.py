"""
Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements pytest-bdd steps for authorization and mounting oneclient.
"""

import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers

import subprocess
import time
import os


from environment import docker, env
from environment import common as env_common
from common import *


@given(parsers.parse('{users} start oneclients {client_instances} in\n' +
                     '{mount_paths} on client_hosts\n' +
                     '{client_hosts} respectively,\n' +
                     'using {tokens}'))
def multi_mount(users, client_instances, mount_paths, client_hosts, tokens, environment, context, client_ids):

    users = list_parser(users)
    client_instances = list_parser(client_instances)
    mount_paths = list_parser(mount_paths)
    client_hosts = list_parser(client_hosts)
    tokens = list_parser(tokens)

    # current version is for environment with one OZ
    oz_node = environment['oz_worker_nodes'][0]

    set_dns(environment)

    client_data = environment['client_data']
    clients = create_clients(users, client_hosts, mount_paths, client_ids)
    clean_spaces_all_users(users, clients)

    parameters = zip(users, clients, client_instances, mount_paths, client_hosts, tokens)
    for user, client, client_instance, mount_path, client_host, token_arg in parameters:
        data = client_data[client_host][client_instance]

        # get OZ cookie from env description file
        cookie = get_cookie(context.env_path, oz_node)
        # get token for user
        token = get_token(token_arg, user, oz_node, cookie)

        # clean if there is directory in the mount_path
        if run_cmd(user, client, "ls " + mount_path) == 0:
            clean_mount_path(user, client)

        # /root has to be accessible for gdb to access /root/bin/oneclient
        assert run_cmd('root', client, 'chmod +x /root') == 0

        token_path = "/tmp/token"

        cmd = ('mkdir -p {mount_path}'
               ' && export GLOBAL_REGISTRY_URL={gr_domain}'
               ' && export PROVIDER_HOSTNAME={op_domain}'
               ' && export X509_USER_CERT={user_cert}'
               ' && export X509_USER_KEY={user_key}'
               ' && echo {token} > {token_path}'
               ' && gdb oneclient -batch -return-child-result -ex \'run --authentication token --no_check_certificate {mount_path} < {token_path}\' -ex \'bt\' 2>&1'
               ' && rm {token_path}').format(
                    mount_path=mount_path,
                    gr_domain=data['zone_domain'],
                    op_domain=data['op_domain'],
                    user_cert=data['user_cert'],
                    user_key=data['user_key'],
                    user=user,
                    token=token,
                    token_path=token_path)

        ret = run_cmd(user, client, cmd)

        if token_arg != "bad token":
            # if token was different than "bad token", check if logging succeeded
            assert ret == 0

        if user in context.users:
            context.users[user].clients[client_instance] = client
        else:
            context.users[user] = User(client_instance, client)

        # remove accessToken to mount many clients on one docker
        run_cmd(user, client,
                "rm -rf " + os.path.join(os.path.dirname(mount_path), ".local"))

        save_op_code(context, user, ret)

    # TODO This is just a temporary solution, delete it after resolving VFS-1881
    if context.env_json not in ['env.json', 'env2.json']:
        time.sleep(5)



@given(parsers.parse('{user} starts oneclient in {mount_path} using {token}'))
def default_mount(user, mount_path, token, environment, context, client_ids):
    multi_mount(make_arg_list(user), make_arg_list("client1"), make_arg_list(mount_path),
                make_arg_list('client_host_1'), make_arg_list(token), environment, context, client_ids)


@then(parsers.parse('{spaces} are mounted for {user}'))
def check_spaces(spaces, user, context):
    time.sleep(3)
    # sleep to be sure that environment is up
    spaces = list_parser(spaces)
    user = str(user)
    for client_instance, client in context.users[user].clients.items():
        spaces_in_client = run_cmd(user, client, 'ls ' + make_path("spaces", client), output=True)
        spaces_in_client = spaces_in_client.split("\n")
        for space in spaces:
            assert space in spaces_in_client


####################################################################################################


def create_clients(users, client_hosts, mount_paths, client_ids):
    clients = []
    params = zip(users, client_hosts, mount_paths)
    for user, client_host, mount_path in params:
        clients.append(Client(client_ids[client_host], mount_path))
    return clients


def clean_spaces_all_users(users, clients):
    params = zip(users, clients)
    for user, client in params:
        clean_spaces(user, client)
    time.sleep(10)


def clean_spaces(user, client):
    # if directory spaces exists
    if run_cmd(user, client, 'ls ' + make_path('spaces', client)) == 0:
        spaces = run_cmd(user, client, 'ls ' + make_path('spaces', client), output=True)
        spaces = spaces.split("\n")
        # clean spaces
        for space in spaces:
            run_cmd(user, client, "rm -rf " + make_path('spaces/' + space + '/*', client))


def clean_mount_path(user, client):

    clean_spaces(user, client)
    # get pid of running oneclient node
    pid = run_cmd('root', client,
                  " | ".join(
                      ["ps aux",
                       "grep './oneclient --authentication token --no_check_certificate '" + client.mount_path,
                       "grep -v 'grep'",
                       "awk '{print $2}'"]),
                  output=True)

    if pid != "":
        # kill oneclient process
        run_cmd("root", client, "kill -KILL " + str(pid))

    # unmount onedata
    run_cmd(user, client, "fusermount -u " + client.mount_path)

    # remove onedata dir
    run_cmd("root", client, "rm -rf " + client.mount_path)


def set_dns(environment):
    with open("/etc/resolv.conf", "w") as conf:
        dns = environment['dns']
        conf.write("nameserver " + dns)


def get_token(token, user, oz_node, cookie):
    if token == "bad token":
        token = "bad_token"
    elif token == "token":
        token = subprocess.check_output(
            ['./tests/cucumber/scenarios/utils/get_token.escript', oz_node, user, cookie],
            stderr=subprocess.STDOUT)
    return token


def get_cookie(config_path, oz_node):
    config = env_common.parse_json_config_file(config_path)
    oz_domain = config['zone_domains'].keys()[0]
    cm = config['zone_domains'][oz_domain]['cluster_manager'].keys()[0]
    return str(config['zone_domains'][oz_domain]['cluster_manager'][cm]['vm.args']['setcookie'])
