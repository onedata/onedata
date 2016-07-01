"""Module implements pytest-bdd steps for operations on regular files.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.docker_utils import run_cmd
from cucumber_utils import *
from tests.utils.client_utils import (cp, truncate, dd, cat,
                                      md5sum, replace_pattern, get_client,
                                      save_op_code, open_file, close_file,
                                      read_from_offset, write_to_opened_file,
                                      read_from_opened_file, write, read,
                                      execute)
import subprocess


@when(parsers.parse('{user} writes "{data}" at offset {offset} to {file} on {client_node}'))
def write_at_offset(user, data, offset, file, client_node, context):
    client = get_client(client_node, user, context)
    path = client.absolute_path(file)
    write_command = '''python -c "with open(\\"{path}\\", \\"r+b\\") as file:
    file.seek({offset})
    file.write(\\"{data}\\")"
'''.format(path=path, offset=offset, data=data)
    ret = run_cmd(user, client, write_command)
    save_op_code(context, user, ret)


@when(parsers.parse('{user_name} writes {megabytes} MB of random characters to {file} on {client_node} and saves MD5'))
@then(parsers.parse('{user_name} writes {megabytes} MB of random characters to {file} on {client_node} and saves MD5'))
def write_rand_text(user_name, megabytes, file, client_node, context):
    user = context.get_user(user_name)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)
    dd(client, megabytes, 1, file_path, user=user_name, output=False) # todo save op code
    try:
        context.md5 = md5sum(client, file_path)
        user.mark_last_operation_succeeded()
    except:
        user.mark_last_operation_failed()


@when(parsers.parse('{user} writes "{text}" to previously opened {file} on {client_node}'))
@then(parsers.parse('{user} writes "{text}" to previously opened {file} on {client_node}'))
def write_opened(user, text, file, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client.absolute_path(file)
    write_to_opened_file(client, file_path, text)


@when(parsers.parse('{user} writes "{text}" to {file} on {client_node}'))
@then(parsers.parse('{user} writes "{text}" to {file} on {client_node}'))
def write_text(user, text, file, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)
    try:
        write(client, str(text), file_path)
        user.mark_last_operation_succeeded()
    except:
        user.mark_last_operation_failed()


@when(parsers.parse('{user} reads "{text}" from previously opened file {file} on {client_node}'))
@then(parsers.parse('{user} reads "{text}" from previously opened file {file} on {client_node}'))
def read_opened(user, text, file, client_node, context):
    client = get_client(client_node, user, context)
    text = text.decode('string_escape')

    def condition():

        try:
            read_text = read_from_opened_file(client, client.absolute_path(file))
            return read_text == text
        except:
            return False

    assert client.perform(condition)


@when(parsers.parse('{user} reads "{text}" from offset {offset} in file {file} on {client_node}'))
@then(parsers.parse('{user} reads "{text}" from offset {offset} in file {file} on {client_node}'))
def read_text_from_offset(user, text, file, offset, client_node, context):
    client = get_client(client_node, user, context)     #todo zrobic step do przewijania, osobny
    text = text.decode('string_escape')

    def condition():

        try:
            read_text = read_from_offset(client,
                                         client.absolute_path(file),
                                         int(offset))
            return read_text == text
        except subprocess.CalledProcessError:
            return False

    assert client.perform(condition)


@when(parsers.parse('{user} reads "{text}" from file {file} on {client_node}'))
@then(parsers.parse('{user} reads "{text}" from file {file} on {client_node}'))
def read_text(user, text, file, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)
    
    def condition():

        try:
            read_text = read(client, file_path)
            return read_text == text
        except:
            return False

    assert client.perform(condition)


@then(parsers.parse('{user} reads "" from {file} on {client_node}'))
def read_empty(user, file, client_node, context):
    read_text(user, '', file, client_node, context)


@then(parsers.parse('{user} cannot read from {file} on {client_node}'))
def cannot_read(user, file, client_node, context):
    client = get_client(client_node, user, context)
    return_code = cat(client, client.absolute_path(file), user=user,
                      output=False)
    assert return_code != 0


@when(parsers.parse('{user} appends "{text}" to {file} on {client_node}'))
def append(user, text, file, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)
    try:
        write(client, str(text), file_path, mode='a')
        user.mark_last_operation_succeeded()
    except:
        user.mark_last_operation_failed()


@when(parsers.parse('{user} replaces "{text1}" with "{text2}" in {file} on {client_node}'))
def replace(user, text1, text2, file, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client.absolute_path(file)
    ret = replace_pattern(client, file_path, text1, text2, user)
    save_op_code(context, user, ret) # todo remove get_client, and save_op_code


@when(parsers.parse('{user} executes {script} on {client_node}'))
@then(parsers.parse('{user} executes {script} on {client_node}'))
def execute_script(user, script, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    script_path = client.absolute_path(script)
    try:
        execute(client, script_path)
        user.mark_last_operation_succeeded()
    except:
        user.mark_last_operation_failed()


@when(parsers.parse('{user} checks MD5 of {file} on {client_node}'))
@then(parsers.parse('{user} checks MD5 of {file} on {client_node}'))
def check_md5(user, file, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)

    def condition():
        try:
            md5 = md5sum(client, client.absolute_path(file))
            return md5 == context.md5
        except:
            return False

    assert client.perform(condition)


@when(parsers.parse('{user} copies regular file {file} to {path} on {client_node}'))
def copy_reg_file(user, file, path, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src_path = client.absolute_path(file)
    dest_path = client.absolute_path(path)
    try:
        cp(client, src_path, dest_path)
        user.mark_last_operation_succeeded()
    except Exception as e:
        print "COPY: ", e
        user.mark_last_operation_failed()


@when(parsers.parse('{user} changes {file} size to {new_size} bytes on {client_node}'))
def do_truncate(user, file, new_size, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)
    try:
        truncate(client, file_path, int(new_size))
        print "FILE WAS TRUNCATED"
        user.mark_last_operation_succeeded()
    except Exception as e:
        print "TRUNCATING FAILED", e
        user.mark_last_operation_failed()


@when(parsers.parse('{user} opens {file} with mode {mode} on {client_node}'))
@then(parsers.parse('{user} opens {file} with mode {mode} on {client_node}'))
def open(user, file, mode, client_node, context):
    client = get_client(client_node, user, context)
    file_path = client.absolute_path(file)
    open_file(client, file_path, mode)


@when(parsers.parse('{user} closes {file} on {client_node}'))
@then(parsers.parse('{user} closes {file} on {client_node}'))
def close(user, file, client_node, context):
    client = context.get_client(user, client_node)
    file_path = client.absolute_path(file)
    close_file(client, file_path)

