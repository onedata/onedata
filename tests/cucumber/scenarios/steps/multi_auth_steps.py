"""
Author: Piotr Ociepka
Author: Jakub Kudzia
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Module implements pytest-bdd steps for authorization and mounting oneclient.
"""
from environment import docker, env
from environment.common import parse_json_config_file
from common import *
import multi_file_steps

from pytest_bdd import given, then
from pytest_bdd import parsers

import os
import time
import subprocess
import pytest


@given(parsers.parse('{users} start oneclients {client_instances} in\n' +
                     '{mount_paths} on client_hosts\n' +
                     '{client_hosts} respectively,\n' +
                     'using {tokens}'))
def multi_mount(users, client_instances, mount_paths, client_hosts, tokens,
                request, environment, context, client_ids,
                env_description_file):

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

    def fin():
        params = zip(users, clients)
        for user, client in params:
            clean_mount_path(user, client)

    request.addfinalizer(fin)

    parameters = zip(users, clients, client_instances, mount_paths, client_hosts, tokens)
    for user, client, client_instance, mount_path, client_host, token_arg in parameters:
        data = client_data[client_host][client_instance]

        # get OZ cookie from env description file
        cookie = get_cookie(env_description_file, oz_node)
        # get token for user
        token = get_token(token_arg, user, oz_node, cookie)
        client.set_timeout(data.get('default_timeout', 0))

        print "User {user} mounts oneclient using token: {token}"\
            .format(
                user=user,
                token=token)

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
               ).format(
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
        rm(client, recursive=True, force=True,
           path=os.path.join(os.path.dirname(mount_path), ".local"))

        rm(client, recursive=True, force=True, path=token_path)

        time.sleep(10)
        if token != 'bad_token':
            clean_spaces(user, client)
        save_op_code(context, user, ret)


@then(parsers.parse('{spaces} are mounted for {user}'))
def check_spaces(spaces, user, context):
    # sleep to be sure that environment is up
    time.sleep(10)
    spaces = list_parser(spaces)
    user = str(user)
    for client_instance, client in context.users[user].clients.items():
        spaces_path = make_path("spaces", client)

        def condition():

            try:
                spaces_in_client = ls(client, path=spaces_path, user=user)
                spaces_in_client = spaces_in_client.split("\n")
                for space in spaces:
                    if space not in spaces_in_client:
                        return False
                    return True
            except subprocess.CalledProcessError:
                return False

        repeat_until(condition, timeout=client.timeout)


@given(parsers.parse('oneclient is started for {users} on {clients}'))
def is_oneclient_started_multi(users, clients, context):
    users = list_parser(users)
    clients = list_parser(clients)
    params = zip(users, clients)
    for user, client in params:
        multi_file_steps.ls_present(user, make_arg_list('spaces'), '.', client,
                                    context)


####################################################################################################


def create_clients(users, client_hosts, mount_paths, client_ids):
    clients = []
    params = zip(users, client_hosts, mount_paths)
    for user, client_host, mount_path in params:
        clients.append(Client(client_ids[client_host], mount_path))
    return clients


def clean_spaces(user, client):
    try:
        spaces = ls(client, user=user, path=make_path('spaces', client))
        spaces = spaces.split("\n")
        # clean spaces
        for space in spaces:
                rm(client, recursive=True, user=user, force=True,
                   path=make_path(os.path.join('spaces', str(space), '*'),
                                  client))
    except:
        pytest.skip("Test skipped beacause of failing to clean spaces")


def clean_spaces_safe(user, client):
    spaces = ls(client, user=user, path=make_path('spaces', client))
    spaces = spaces.split("\n")
    # clean spaces
    for space in spaces:
        rm(client, recursive=True, user=user, force=True,
           path=make_path(os.path.join('spaces', str(space), '*'), client))


def clean_mount_path(user, client):
    try:
        clean_spaces_safe(user, client)
    except:
        pass
    finally:
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
        fusermount(client, client.mount_path, user=user, unmount=True,
                   lazy=True)
        # run_cmd(user, client, "fusermount -ul " + client.mount_path)
        # remove onedata dir
        rm(client, recursive=True, force=True, path=client.mount_path, output=True)


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
    config = parse_json_config_file(config_path)
    oz_domain = config['zone_domains'].keys()[0]
    cm = config['zone_domains'][oz_domain]['cluster_manager'].keys()[0]
    return str(config['zone_domains'][oz_domain]['cluster_manager'][cm]['vm.args']['setcookie'])
