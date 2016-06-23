"""Module implements pytest-bdd steps for operations on regular files.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from tests.utils.docker_utils import run_cmd
from cucumber_utils import *
from tests.utils.client_utils import (cp, truncate, dd, echo_to_file, cat,
                                      md5sum, replace_pattern, get_client,
                                      client_mount_path, save_op_code,
                                      open_file, close_file, read_from_offset,
                                      write_to_opened_file,
                                      read_from_opened_file)
import subprocess


@when(parsers.parse('{user} writes "{data}" at offset {offset} to {file} on {client_node}'))
def write_at_offset(user, data, offset, file, client_node, context):
    client = get_client(client_node, user, context)
    path = client_mount_path(file, client)
    write_command = '''python -c "with open(\\"{path}\\", \\"r+b\\") as file:
    file.seek({offset})
    file.write(\\"{data}\\")"
'''.format(path=path, offset=offset, data=data)
    ret = run_cmd(user, client, write_command)
    save_op_code(context, user, ret)


@when(parsers.parse('{user} writes {megabytes} MB of random characters to {file} on {client_node} and saves MD5'))
@then(parsers.parse('{user} writes {megabytes} MB of random characters to {file} on {client_node} and saves MD5'))
def write_rand_text(user, megabytes, file, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client_mount_path(file, client)
    ret = dd(client, megabytes, 1, file_path, user=user, output=False)
    md5 = md5sum(client, file_path, user=user)
    context.md5 = md5.split()[0]
    save_op_code(context, user, ret)


@when(parsers.parse('{user} writes "{text}" to previously opened {file} on {client_node}'))
@then(parsers.parse('{user} writes "{text}" to previously opened {file} on {client_node}'))
def write_opened(user, text, file, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client_mount_path(file, client)
    write_to_opened_file(client, file_path, text)


@when(parsers.parse('{user} writes "{text}" to {file} on {client_node}'))
@then(parsers.parse('{user} writes "{text}" to {file} on {client_node}'))
def write_text(user, text, file, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client_mount_path(file, client)
    ret = echo_to_file(client, str(text), file_path, escape=True, user=user)
    save_op_code(context, user, ret)


@when(parsers.parse('{user} reads "{text}" from previously opened file {file} on {client_node}'))
@then(parsers.parse('{user} reads "{text}" from previously opened file {file} on {client_node}'))
def read_opened(user, text, file, client_node, context):
    client = get_client(client_node, user, context)
    text = text.decode('string_escape')

    def condition():

        try:
            read_text = read_from_opened_file(client, client_mount_path(file, client))
            return read_text == text
        except:
            return False

    assert repeat_until(condition, client.timeout)


@when(parsers.parse('{user} reads "{text}" from offset {offset} in file {file} on {client_node}'))
@then(parsers.parse('{user} reads "{text}" from offset {offset} in file {file} on {client_node}'))
def read_text(user, text, file, offset, client_node, context):
    client = get_client(client_node, user, context)
    text = text.decode('string_escape')

    def condition():

        try:
            read_text = read_from_offset(client,
                                         client_mount_path(file, client),
                                         int(offset))
            return read_text == text
        except subprocess.CalledProcessError:
            return False

    assert repeat_until(condition, client.timeout)


@when(parsers.parse('{user} reads "{text}" from file {file} on {client_node}'))
@then(parsers.parse('{user} reads "{text}" from file {file} on {client_node}'))
def read_text(user, text, file, client_node, context):
    client = get_client(client_node, user, context)
    text = text.decode('string_escape')

    def condition():

        try:
            read_text = cat(client, client_mount_path(file, client), user=user)
            return read_text == text
        except subprocess.CalledProcessError:
            return False

    assert repeat_until(condition, client.timeout)


@then(parsers.parse('{user} reads "" from {file} on {client_node}'))
def read_empty(user, file, client_node, context):
    read_text(user, '', file, client_node, context)


@then(parsers.parse('{user} cannot read from {file} on {client_node}'))
def cannot_read(user, file, client_node, context):
    client = get_client(client_node, user, context)
    return_code = cat(client, client_mount_path(file, client), user=user,
                      output=False)
    assert return_code != 0


@when(parsers.parse('{user} appends "{text}" to {file} on {client_node}'))
def append(user, text, file, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client_mount_path(file, client)
    ret = echo_to_file(client, str(text), file_path, user=user, overwrite=False)
    save_op_code(context, user, ret)


@when(parsers.parse('{user} replaces "{text1}" with "{text2}" in {file} on {client_node}'))
def replace(user, text1, text2, file, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client_mount_path(file, client)
    ret = replace_pattern(client, file_path, text1, text2, user)
    save_op_code(context, user, ret)


@when(parsers.parse('{user} executes {file} on {client_node}'))
@then(parsers.parse('{user} executes {file} on {client_node}'))
def execute_script(user, file, client_node, context):
    client = get_client(client_node, user, context)
    ret = run_cmd(user, client, client_mount_path(file, client))
    save_op_code(context, user, ret)


@when(parsers.parse('{user} checks MD5 of {file} on {client_node}'))
@then(parsers.parse('{user} checks MD5 of {file} on {client_node}'))
def check_md5(user, file, client_node, context):
    client = get_client(client_node, user, context)

    def condition():
        try:
            md5 = md5sum(client, client_mount_path(file, client), user=user)
            return md5.split()[0] == context.md5
        except subprocess.CalledProcessError:
            return False

    assert repeat_until(condition, client.timeout)


@when(parsers.parse('{user} copies regular file {file} to {path} on {client_node}'))
def copy_reg_file(user, file, path, client_node, context):
    client = get_client(client_node, user, context)
    src_path = client_mount_path(file, client)
    dest_path = client_mount_path(path, client)
    ret = cp(client, src_path, dest_path, user=user)
    save_op_code(context, user, ret)


@when(parsers.parse('{user} changes {file} size to {new_size} bytes on {client_node}'))
def do_truncate(user, file, new_size, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client_mount_path(file, client)
    ret = truncate(client, file_path, new_size, user=user)
    save_op_code(context, user, ret)


@when(parsers.parse('{user} opens {file} with mode {mode} on {client_node}'))
@then(parsers.parse('{user} opens {file} with mode {mode} on {client_node}'))
def open(user, file, mode, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client_mount_path(file, client)
    open_file(client, file_path, mode)


@when(parsers.parse('{user} closes {file} on {client_node}'))
@then(parsers.parse('{user} closes {file} on {client_node}'))
def close(user, file, client_node, context):
    client = context.get_client(user, client_node)
    file_path = client_mount_path(file, client)
    close_file(client, file_path)

