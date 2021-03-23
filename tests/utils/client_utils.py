"""This module contains utility functions for using client instances under
tests. Client is started in docker during env_up, acceptance and performance
tests.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.docker_utils import run_cmd
from tests.utils.path_utils import escape_path
from tests.utils.user_utils import User
from tests.utils.utils import (set_dns, get_token, get_oz_cookie,
                               log_exception, assert_)
import errno

import pytest
import os
import time
import rpyc
import stat as stat_lib
import hashlib

TOKEN_PATH = '/tmp/token'


class Client:
    def __init__(self, docker_name, mount_path):
        self.timeout = 0
        self.docker_name = docker_name
        self.mount_path = mount_path
        self.rpyc_connection = None
        self.opened_files = {}
        self.file_stats = {}
        self.rpyc_server_pid = None
        self.user_cert = None
        self.user_key = None
        self.token = None

    def set_timeout(self, timeout):
        self.timeout = timeout

    def mount(self, user):

        ret = oneclient(user.name, self.mount_path, user.oz_domain,
                        user.provider.domain, self.user_cert, self.user_key,
                        self, TOKEN_PATH, token=self.token)

        rm(self, TOKEN_PATH, force=True)
        # remove accessToken to mount many clients on one docker
        rm(self, path=os.path.join(os.path.dirname(self.mount_path), ".local"),
           recursive=True, force=True)
        return ret

    def remount(self, user):
        fusermount(self, self.mount_path, user.name, unmount=True)
        return self.mount(user)

    def start_rpyc(self, user):
        self._start_rpyc_server(user)
        started = False
        timeout = 30
        while not started and timeout >= 0:
            try:
                self.rpyc_connection = rpyc.classic.connect(self.docker_name)
                started = True
            except Exception as e:
                print e
                time.sleep(1)
                timeout -= 1
        if not started:
            pytest.fail("rpc connection couldn't be established")

    def stop_rpyc_server(self):
        if self.rpyc_server_pid:
            kill(self, self.rpyc_server_pid)
            self.rpyc_server_pid = None

    def perform(self, condition, timeout=None):
        if timeout is None:
            timeout = self.timeout
        return self._repeat_until(condition, timeout)

    def _start_rpyc_server(self, user_name):    #start rpc server on client docker
        cmd = "/usr/local/bin/rpyc_classic.py --host=0.0.0.0 --port=18812"
        run_cmd(user_name, self, cmd, detach=True)
        pid = run_cmd(user_name,
                      self,
                      " | ".join(["ps -u {}".format(user_name),
                                  "grep 'rpyc_classic.py'",
                                  "grep -v 'grep'",
                                  "awk '{print $1}'"]),
                      output=True)

        self.rpyc_server_pid = pid

    @staticmethod
    def _repeat_until(condition, timeout):
        condition_satisfied = False
        while not condition_satisfied and timeout >= 0:
            try:
                condition_satisfied = condition()
                if condition_satisfied is None:
                    condition_satisfied = True
            except:
                log_exception()
                condition_satisfied = False
            finally:
                time.sleep(1)
                timeout -= 1

        return timeout >= 0 or condition_satisfied

    def absolute_path(self, path):
        return os.path.join(self.mount_path, str(path))


def mount_users(request, environment, context, client_dockers,
                env_description_file, providers, user_names=[],
                client_instances=[], mount_paths=[], client_hosts=[],
                tokens=[]):

    # current version is for environment with one OZ
    oz_node = environment['oz_worker_nodes'][0]

    set_dns(environment)

    client_data = environment['client_data']
    clients = create_clients(user_names, client_hosts, mount_paths, client_dockers)

    parameters = zip(user_names, clients, client_instances, mount_paths,
                     client_hosts, tokens)
    for user_name, client, client_instance, mount_path, client_host, token_arg in parameters:
        data = client_data[client_host][client_instance]

        oz_domain = data['zone_domain']
        # get OZ cookie from env description file
        cookie = get_oz_cookie(env_description_file, oz_domain, node_name=False)

        user = context.get_user(user_name)
        if not user:
            provider_id = data['op_domain'].split('.')[0]
            user = User(user_name,
                        id=user_name,
                        oz_domain=oz_domain,
                        provider=providers[provider_id])

        # get token for user
        if token_arg != 'bad_token':
            token = get_token(token_arg, user.id, oz_node, cookie)

        client.set_timeout(data.get('default_timeout', 0))

        print "User {user} mounts oneclient using token: {token}"\
            .format(user=user_name, token=token)

        # /root has to be accessible for gdb to access /root/bin/oneclient
        assert run_cmd('root', client, 'chmod +x /root') == 0

        client.start_rpyc(user.name)
        client.token = token
        client.user_cert = data['user_cert']
        client.user_key = data['user_key']
        ret = client.mount(user)

        user.update_clients(client_instance, client)
        if not context.has_user(user):
            context.add_user(user)

        if ret != 0 and token_arg != "bad token":
            # if token was different than "bad token" and mounting failed
            clean_mount_path(user_name, client)
            pytest.fail("Error mounting oneclient")

        # NOTE without this sleep protocol error occurs more often during cleaning spaces
        time.sleep(3)

        if token != 'bad_token':
            try:
                clean_spaces(client)
            except AssertionError:
                pytest.fail("Failed to clean spaces")

        if ret == 0:
            user.mark_last_operation_succeeded()
        else:
            user.mark_last_operation_failed()

    def fin():
        params = zip(user_names, clients)
        for user_name, client in params:
            time.sleep(5)
            for opened_file in client.opened_files.keys():
                close_file(client, opened_file)
            client.opened_files.clear()
            clean_mount_path(user_name, client)
        for user_name in user_names:
            user = context.get_user(user_name)
            for client in user.clients.values():
                client.stop_rpyc_server()
            user.clients.clear()

    request.addfinalizer(fin)


def oneclient(user_name, mount_path, oz_domain, op_domain, user_cert, user_key,
              client, token_path, token, gdb=False):

    cmd = None
    if gdb:
        cmd = ('mkdir -p {mount_path}'
            ' && export ONECLIENT_PROVIDER_HOST={op_domain}'
            ' && echo {token} > {token_path}'
            ' && gdb oneclient -batch -return-child-result -ex \'run --log-dir /tmp --insecure {mount_path} < {token_path}\' -ex \'bt\' 2>&1'
            ).format(mount_path=mount_path,
                        op_domain=op_domain,
                        token=token,
                        token_path=token_path)
    else:
        cmd = ('mkdir -p {mount_path}'
            ' && export ONECLIENT_PROVIDER_HOST={op_domain}'
            ' && echo {token} > {token_path}'
            ' && ./oneclient --log-dir /tmp --insecure {mount_path} < {token_path} 2>&1'
            ).format(mount_path=mount_path,
                        op_domain=op_domain,
                        token=token,
                        token_path=token_path)

    return run_cmd(user_name, client, cmd)


def ls(client, path="."):
    return client.rpyc_connection.modules.os.listdir(path)


def osrename(client, src, dest):
    client.rpyc_connection.modules.os.rename(src, dest)


def mv(client, src, dest):
    client.rpyc_connection.modules.shutil.move(src, dest)


def chmod(client, mode, file_path):
    client.rpyc_connection.modules.os.chmod(file_path, mode)


def stat(client, path):
    return client.rpyc_connection.modules.os.stat(path)


def rm(client, path, recursive=False, force=False, onerror=None):
    if recursive and force:
        client.rpyc_connection.modules.shutil.rmtree(path, ignore_errors=True, onerror=onerror)
    elif recursive:
        client.rpyc_connection.modules.shutil.rmtree(path, onerror=onerror)
    else:
        client.rpyc_connection.modules.os.remove(path)


def rmdir(client, dir_path, recursive=False):

    if recursive:
        client.rpyc_connection.modules.os.removedirs(dir_path)
    else:
        client.rpyc_connection.modules.os.rmdir(dir_path)


def mkdir(client, dir_path, recursive=False):

    if recursive:
        client.rpyc_connection.modules.os.makedirs(dir_path)
    else:
        client.rpyc_connection.modules.os.mkdir(dir_path)


def create_file(client, file_path, mode=0664):
    client.rpyc_connection.modules.os.mknod(file_path, mode | stat_lib.S_IFREG)


def touch(client, file_path):
    client.rpyc_connection.modules.os.utime(file_path, None)


def cp(client, src, dest, recursive=False):
    if recursive:
        if client.rpyc_connection.modules.os.path.isdir(dest):
            # shutil.copytree fails if dest is an existing directory
            dest = os.path.join(dest, os.path.basename(os.path.normpath(src)))
            client.rpyc_connection.modules.shutil.copytree(src, dest)
        else:
            client.rpyc_connection.modules.shutil.copytree(src, dest)
    else:
        client.rpyc_connection.modules.shutil.copy(src, dest)


def truncate(client, file_path, size):
    with client.rpyc_connection.builtins.open(file_path, 'w') as f:
        f.truncate(size)


def write(client, text, file_path, mode='w'):
    with client.rpyc_connection.builtins.open(file_path, mode) as f:
        f.write(text)


def read(client, file_path, mode='r'):
    with client.rpyc_connection.builtins.open(file_path, mode) as f:
        read_text = f.read()
    return read_text


def open_file(client, file, mode='w+'):
    return client.rpyc_connection.builtins.open(file, mode)


def close_file(client, file):
    client.opened_files[file].close()


def write_to_opened_file(client, file, text):
    client.opened_files[file].write(text)
    client.opened_files[file].flush()


def read_from_opened_file(client, file):
    return client.opened_files[file].read()


def seek(client, file, offset):
    client.opened_files[file].seek(offset)

def setxattr(client, file, name, value):
    xattrs = client.rpyc_connection.modules.xattr.xattr(file)
    xattrs[name] = value

def getxattr(client, file, name):
    xattrs = client.rpyc_connection.modules.xattr.xattr(file)
    return xattrs[name]

def listxattr(client, file):
    xattrs = client.rpyc_connection.modules.xattr.xattr(file)
    return xattrs.list()

def removexattr(client, file, name):
    xattrs = client.rpyc_connection.modules.xattr.xattr(file)
    del xattrs[name]

def execute(client, command, output=False):
    if output:
        return client.rpyc_connection.modules.subprocess.check_output(command)
    else:
        return client.rpyc_connection.modules.subprocess.call(command)


def md5sum(client, file_path):
    m = hashlib.md5()
    with client.rpyc_connection.builtins.open(file_path, 'r') as f:
        m.update(f.read())
    return m.hexdigest()


def mkstemp(client, dir=None):

    _handle, abs_path = client.rpyc_connection.modules.tempfile.mkstemp(dir=dir)
    return abs_path


def mkdtemp(client, dir=None):
    return client.rpyc_connection.modules.tempfile.mkdtemp(dir=dir)


def replace_pattern(client, file_path, pattern, new_text, user='root',
                    output=False):
    cmd = 'sed -i \'s/{pattern}/{new_text}/g\' {file_path}'\
        .format(pattern=pattern,
                new_text=new_text,
                file_path=escape_path(file_path))
    return run_cmd(user, client, cmd, output=output)


def dd(client, block_size, count, output_file, unit='M', input_file="/dev/zero",
       user="root", output=False, error=False):
    cmd = "dd {input} {output} {bs} {count}"\
        .format(input="if={}".format(escape_path(input_file)),
                output="of={}".format(escape_path(output_file)),
                bs="bs={0}{1}".format(block_size, unit),
                count="count={}".format(count))
    return run_cmd(user, client, cmd, output=output, error=error)


def fusermount(client, path, user='root', unmount=False, lazy=False,
               quiet=False, output=False):
    cmd = "fusermount {unmount} {lazy} {quiet} {path}"\
        .format(unmount="-u" if unmount else "",
                lazy="-z" if lazy else "",
                quiet="-q" if quiet else "",
                path=escape_path(path))
    return run_cmd(user, client, cmd, output=output)


def kill(client, pid, signal="KILL", user='root', output=False):
    cmd = "kill -{signal} {pid}".format(signal=signal, pid=pid)
    return run_cmd(user, client, cmd, output=output)


def create_clients(users, client_hosts, mount_paths, client_dockers):
    clients = []
    params = zip(users, client_hosts, mount_paths)
    for user, client_host, mount_path in params:
        clients.append(Client(client_dockers[client_host], mount_path))
    return clients


def clean_spaces(client):
    spaces = ls(client, path=client.mount_path)

    for space in spaces:
        space_path = client.absolute_path(space)

        def condition():
            try:
                rm(client, path=space_path, recursive=True, force=True)
            except Exception as e:
                if isinstance(e, OSError):
                    if e.errno == errno.EACCES:
                       # ignore EACCES errors during cleaning
                        return
                raise
        assert_(client.perform, condition)


def clean_mount_path(user, client):
    try:
        clean_spaces(client)
    except Exception as e:
        pass
    finally:
        # get pid of running oneclient node
        pid = run_cmd('root', client,
                      " | ".join(
                              ["ps aux",
                               "grep './oneclient --insecure '" + client.mount_path,
                               "grep -v 'grep'",
                               "awk '{print $2}'"]),
                      output=True)

        if pid != "":
            # kill oneclient process
            kill(client, pid)

        # unmount onedata
        fusermount(client, client.mount_path, user=user, unmount=True)
        rm(client, path=client.mount_path, recursive=True, force=True)


def user_home_dir(user="root"):
    return os.path.join("/home", user)
