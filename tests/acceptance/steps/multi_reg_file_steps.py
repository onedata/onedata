"""Module implements pytest-bdd steps for operations on regular files.
"""

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import multi_file_steps
from tests.utils.utils import assert_, assert_generic, assert_false
from tests.utils.acceptance_utils import *
from tests.utils.client_utils import (cp, truncate, dd, md5sum, write, read,
                                      replace_pattern, open_file, execute,
                                      close_file, write_to_opened_file,
                                      read_from_opened_file, seek, run_cmd,
                                      escape_path)


@when(parsers.parse('{user_name} writes "{data}" at offset {offset} to {file} on {client_node}'))
def write_at_offset(user_name, data, offset, file, client_node, context):
    user = context.get_user(user_name)
    client = user.get_client(client_node)
    path = client.absolute_path(file)

    def condition():
        f = open_file(client, path, 'r+b')
        f.seek(int(offset))
        f.write(data)
        f.close()

    assert_(client.perform, condition)


@when(parsers.parse('{user_name} writes {megabytes} MB of random characters to {file} on {client_node} and saves MD5'))
@then(parsers.parse('{user_name} writes {megabytes} MB of random characters to {file} on {client_node} and saves MD5'))
def write_rand_text(user_name, megabytes, file, client_node, context):
    user = context.get_user(user_name)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        dd(client, megabytes, 1, file_path, user=user_name, output=True, error=True)
        multi_file_steps.check_size(user_name, file, int(megabytes) * 1024 * 1024, client_node, context)
        count_md5(user_name, file, client_node, context)

    assert_(client.perform, condition, timeout=0)
    user.mark_last_operation_succeeded()


def count_md5(user_name, file_path, client_node, context):
    user = context.get_user(user_name)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file_path)

    def condition():
        context.md5 = md5sum(client, file_path)
        assert context.md5 is not None

    assert_(client.perform, condition)


@when(parsers.parse('{user} writes "{text}" to previously opened {file} on {client_node}'))
@then(parsers.parse('{user} writes "{text}" to previously opened {file} on {client_node}'))
def write_opened(user, text, file, client_node, context):
    client = context.get_client(user, client_node)
    file_path = client.absolute_path(file)
    def condition():
        write_to_opened_file(client, file_path, text)

    assert_(client.perform, condition)


@when(parsers.parse('{user} writes "{text}" to {file} on {client_node}'))
@then(parsers.parse('{user} writes "{text}" to {file} on {client_node}'))
def write_text(user, text, file, client_node, context):
    write_text_base(user, text, file, client_node, context)


@when(parsers.parse('{user} fails to write "{text}" to {file} on {client_node}'))
@then(parsers.parse('{user} fails to write "{text}" to {file} on {client_node}'))
def write_text_fail(user, text, file, client_node, context):
    write_text_base(user, text, file, client_node, context, should_fail=True)


def write_text_base(user, text, file, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        write(client, str(text), file_path)

    assert_generic(client.perform, should_fail, condition, timeout=0)


@when(parsers.parse('{user} reads "{text}" from previously opened file {file} on {client_node}'))
@then(parsers.parse('{user} reads "{text}" from previously opened file {file} on {client_node}'))
def read_opened(user, text, file, client_node, context):
    client = context.get_client(user, client_node)
    text = text.decode('string_escape')

    def condition():
        read_text = read_from_opened_file(client, client.absolute_path(file))
        assert read_text == text

    assert_(client.perform, condition)


@when(parsers.parse('{user} sets current file position in {file} at offset {offset} on {client_node}'))
@then(parsers.parse('{user} sets current file position in {file} at offset {offset} on {client_node}'))
def set_file_position(user, file, offset, client_node, context):
    client = context.get_client(user, client_node)
    file_path = client.absolute_path(file)

    def condition():
        seek(client, file_path, int(offset))

    assert_(client.perform, condition, timeout=0)


@when(parsers.parse('{user} reads "{text}" from file {file} on {client_node}'))
@then(parsers.parse('{user} reads "{text}" from file {file} on {client_node}'))
def read_text(user, text, file, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        read_text = read(client, file_path)
        assert read_text == text

    assert_(client.perform, condition)


@when(parsers.parse('{user} reads "" from file {file} on {client_node}'))
@then(parsers.parse('{user} reads "" from file {file} on {client_node}'))
def read_empty(user, file, client_node, context):
    read_text(user, '', file, client_node, context)


@then(parsers.parse('{user} cannot read from {file} on {client_node}'))
def cannot_read(user, file, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        read(client, file_path)

    assert_false(client.perform, condition)


@when(parsers.parse('{user} appends "{text}" to {file} on {client_node}'))
def append(user, text, file, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        write(client, str(text), file_path, mode='a')

    assert_(client.perform, condition, timeout=0)


@when(parsers.parse('{user} replaces "{text1}" with "{text2}" in {file} on {client_node}'))
def replace(user, text1, text2, file, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        ret = replace_pattern(client, file_path, text1, text2, user.name)
        assert ret == 0

    assert_(client.perform, condition, timeout=0)


@when(parsers.parse('{user} executes {script} on {client_node}'))
@then(parsers.parse('{user} executes {script} on {client_node}'))
def execute_script(user, script, client_node, context):
    execute_script_base(user, script, client_node, context)


@when(parsers.parse('{user} fails to execute {script} on {client_node}'))
@then(parsers.parse('{user} fails to execute {script} on {client_node}'))
def execute_script_fail(user, script, client_node, context):
    execute_script_base(user, script, client_node, context, should_fail=True)


def execute_script_base(user, script, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    script_path = client.absolute_path(script)

    def condition():
        execute(client, script_path)

    assert_generic(client.perform, should_fail, condition, timeout=0)


@when(parsers.parse('{user} checks MD5 of {file} on {client_node}'))
@then(parsers.parse('{user} checks MD5 of {file} on {client_node}'))
def check_md5(user, file, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)

    def condition():
        md5 = md5sum(client, client.absolute_path(file))
        assert md5 == context.md5

    assert_(client.perform, condition)


@when(parsers.parse('{user} copies regular file {file} to {path} on {client_node}'))
def copy_reg_file(user, file, path, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src_path = client.absolute_path(file)
    dest_path = client.absolute_path(path)

    def condition():
        cp(client, src_path, dest_path)

    assert_(client.perform, condition, timeout=0)


@when(parsers.parse('{user} changes {file} size to {new_size} bytes on {client_node}'))
@then(parsers.parse('{user} changes {file} size to {new_size} bytes on {client_node}'))
def do_truncate(user, file, new_size, client_node, context):
    do_truncate_base(user, file, new_size, client_node, context)


@when(parsers.parse('{user} fails to change {file} size to {new_size} bytes on {client_node}'))
@then(parsers.parse('{user} fails to change {file} size to {new_size} bytes on {client_node}'))
def do_truncate_fail(user, file, new_size, client_node, context):
    do_truncate_base(user, file, new_size, client_node, context, should_fail=True)


def do_truncate_base(user, file, new_size, client_node, context, should_fail=False):
    user = context.get_user(user)
    client = user.get_client(client_node)
    file_path = client.absolute_path(file)

    def condition():
        truncate(client, file_path, int(new_size))

    assert_generic(client.perform, should_fail, condition, timeout=0)


@when(parsers.parse('{user} performs command "{command}" in {path} directory on {client_node}'))
@then(parsers.parse('{user} performs command "{command}" in {path} directory on {client_node}'))
def run_cmd_in_directory(user, command, path, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    abs_path = client.absolute_path(path)
    cmd = 'cd {path} && {command}'.format(path=escape_path(abs_path),
                                          command=command)

    def condition():
        ret = run_cmd(user.name, client, cmd, output=True)
        assert ret == 0
    assert_(client.perform, condition, timeout=0)


@when(parsers.parse('{user} opens {file} with mode {mode} on {client_node}'))
@then(parsers.parse('{user} opens {file} with mode {mode} on {client_node}'))
def open(user, file, mode, client_node, context):
    client = context.get_client(user, client_node)
    file_path = client.absolute_path(file)
    f = open_file(client, file_path, mode)
    client.opened_files.update({file_path: f})


@when(parsers.parse('{user} closes {file} on {client_node}'))
@then(parsers.parse('{user} closes {file} on {client_node}'))
def close(user, file, client_node, context):
    client = context.get_client(user, client_node)
    file_path = client.absolute_path(file)
    close_file(client, file_path)
    del client.opened_files[file_path]

