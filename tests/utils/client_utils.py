"""This module contains utility functions for using client instances under
tests. Client is started in docker during acceptance, cucumber and performance
tests.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.utils import set_dns, get_token, get_cookie
from tests.utils.docker_utils import run_cmd
from tests.cucumber.steps.cucumber_utils import repeat_until

import os
import pytest
import subprocess


class User:
    def __init__(self, client_node, client):
        self.clients = {client_node: client}
        self.last_op_ret_code = 0
        self.files = {}


class Client:
    def __init__(self, docker_id, mount_path):
        self.timeout = 0
        self.docker_id = docker_id
        self.mount_path = mount_path

    def set_timeout(self, timeout):
        self.timeout = timeout


def mount_users(request, environment, context, client_ids, env_description_file,
                users=[], client_instances=[], mount_paths=[],
                client_hosts=[], tokens=[]):
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

    parameters = zip(users, clients, client_instances, mount_paths,
                     client_hosts, tokens)
    for user, client, client_instance, mount_path, client_host, token_arg in parameters:
        data = client_data[client_host][client_instance]

        # get OZ cookie from env description file
        cookie = get_cookie(env_description_file, oz_node)
        # get token for user
        if token_arg != 'bad_token':
            token = get_token(token_arg, user, oz_node, cookie)
        client.set_timeout(data.get('default_timeout', 0))

        print "User {user} mounts oneclient using token: {token}".format(
                user=user,
                token=token)

        # /root has to be accessible for gdb to access /root/bin/oneclient
        assert run_cmd('root', client, 'chmod +x /root') == 0

        token_path = "/tmp/token"

        # echo_to_file(client, token, token_path, output=True)

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
            if ret != 0:
                clean_mount_path(user, client)
                pytest.skip("Error mounting oneclient")

        if user in context.users:
            context.users[user].clients[client_instance] = client
        else:
            context.users[user] = User(client_instance, client)

        # remove accessToken to mount many clients on one docker
        rm(client, recursive=True, force=True,
           path=os.path.join(os.path.dirname(mount_path), ".local"))

        rm(client, recursive=True, force=True, path=token_path)
        if token != 'bad_token':
            if not clean_spaces_safe(user, client):
                pytest.skip("Test skipped beacause of failing to clean spaces")

        save_op_code(context, user, ret)


def ls(client, user="root", path=".", output=True):
    """CAUTION: this function returns list of paths not string"""
    cmd = "ls {path}".format(path=path)
    # sometimes paths are separated with 2 spaces, sometimes with '\n\
    return run_cmd(user, client, cmd, output=output).strip() \
        .replace('  ', '\n').split('\n')


def mv(client, src, dest, user="root", output=False):
    cmd = "mv {src} {dest}".format(src=src, dest=dest)
    return run_cmd(user, client, cmd, output=output)


def chmod(client, mode, file, user="root", output=False):
    cmd = "chmod {mode} {file}".format(mode=mode, file=file)
    return run_cmd(user, client, cmd, output=output)


def stat(client, path, format=None, user="root", output=True):
    cmd = "stat {path} {format}".format(
            path=path,
            format="--format='{0}'".format(format) if format else "")
    return run_cmd(user, client, cmd, output=output)


def rm(client, path, recursive=False, force=False, user="root", output=False):
    cmd = "rm {recursive} {force} {path}".format(
            recursive="-r" if recursive else "",
            force="-f" if force else "",
            path=path)
    return run_cmd(user, client, cmd, output=output)


def rmdir(client, dir_path, recursive=False, from_path=None, user="root",
          output=False):
    cmd = ("{from_path}"
           "rmdir {recursive} {path}"
           ).format(
            from_path="cd {0} &&".format(from_path) if from_path else "",
            recursive="-p" if recursive else "",
            path=dir_path)
    return run_cmd(user, client, cmd, output=output)


def mkdir(client, dir_path, recursive=False, user="root", output=False):
    cmd = "mkdir {recursive} {path}".format(

            recursive="-p" if recursive else "",
            path=dir_path)
    return run_cmd(user, client, cmd, output=output)


def touch(client, file_path, user="root", output=False):
    cmd = "touch {path}".format(path=file_path)
    return run_cmd(user, client, cmd, output=output)


def cp(client, src, dest, recursive=False, user="root", output=False):
    cmd = "cp {recursive} {src} {dest}".format(
            recursive="-r" if recursive else "",
            src=src,
            dest=dest)
    return run_cmd(user, client, cmd, output=output)


def truncate(client, file_path, size, user="root", output=False):
    cmd = "truncate --size={size} {file_path}".format(size=size,
                                                      file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def dd(client, block_size, count, output_file, unit='M', input_file="/dev/zero",
       user="root", output=False, error=False):
    cmd = "dd {input} {output} {bs} {count}".format(
            input="if={}".format(input_file),
            output="of={}".format(output_file),
            bs="bs={0}{1}".format(block_size, unit),
            count="count={}".format(count))
    return run_cmd(user, client, cmd, output=output, error=True)


def echo_to_file(client, text, file_path, new_line=False, escape=False,
                 user="root", overwrite=True, output=False):
    cmd = "echo {newline} {escape} '{text}' {redirect} {file_path}".format(
            newline="-n" if not new_line else "",
            escape="-e" if escape else "",
            text=text,
            redirect=">" if overwrite else ">>",
            file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def cat(client, file_path, user="root", output=True):
    cmd = "cat {file_path}".format(file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def md5sum(client, file_path, user="root", output=True):
    cmd = "md5sum {file_path}".format(file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def mktemp(client, path=None, dir=False, user="root", output=True):
    cmd = "mktemp {dir} {path}".format(
            dir="--directory" if dir else "",
            path="--tmpdir={}".format(path) if path else "")
    return run_cmd(user, client, cmd, output=output).strip()


def replace_pattern(client, file_path, pattern, new_text, user='root',
                    output=False):
    cmd = 'sed -i \'s/{pattern}/{new_text}/g\' {file_path}'.format(
            pattern=pattern,
            new_text=new_text,
            file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def fusermount(client, path, user='root', unmount=False, lazy=False,
               quiet=False, output=False):
    cmd = "fusermount {unmount} {lazy} {quiet} {path}".format(
            unmount="-u" if unmount else "",
            lazy="-z" if lazy else "",
            quiet="-q" if quiet else "",
            path=path
    )
    return run_cmd(user, client, cmd, output=output)


def grep(client, expression, user='root', no_grep=True, output=True):
    cmd = "grep '{expression}' {no_grep}".format(
        expression=expression,
        no_grep="| grep -v grep" if no_grep else ""
    )
    return run_cmd(user, client, cmd, output=output)


def create_clients(users, client_hosts, mount_paths, client_ids):
    clients = []
    params = zip(users, client_hosts, mount_paths)
    for user, client_host, mount_path in params:
        clients.append(Client(client_ids[client_host], mount_path))
    return clients


def clean_spaces_safe(user, client):
    def condition():
        try:
            clean_spaces(user, client)
            return True
        except subprocess.CalledProcessError:
            return False

    return repeat_until(condition, 5)


def clean_spaces(user, client):
    spaces = ls(client, user=user, path=client.mount_path)
    # clean spaces
    for space in spaces:
        rm(client, recursive=True, user=user, force=True,
           path=client_mount_path(os.path.join(str(space), '*'),
                                  client))


def clean_mount_path(user, client):
    try:
        clean_spaces(user, client)
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
        fusermount(client, client.mount_path, user=user, unmount=True)
        # lazy=True)
        rm(client, recursive=True, force=True, path=client.mount_path)


def client_mount_path(path, client):
    return os.path.join(client.mount_path, str(path))


def save_op_code(context, user, op_code):
    context.users[user].last_op_ret_code = op_code


def get_client(client_node, user, context):
    return context.users[user].clients[client_node]


def user_home_dir(user="root"):
    return os.path.join("/home", user)
