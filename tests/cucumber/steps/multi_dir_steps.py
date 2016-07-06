"""Module implements pytest-bdd steps for operations on directories in multiclient environment.
"""

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.utils import get_function_name, handle_exception
from tests.utils.cucumber_utils import *
from tests.utils.client_utils import ls, rm, rmdir, mkdir, cp


@when(parsers.parse('{user} creates directories {dirs} on {client_node}'))
@when(parsers.parse('{user} creates directories {dirs}\non {client_node}'))
def create(user, dirs, client_node, context):
    dirs = list_parser(dirs)
    user = context.get_user(user)
    client = user.get_client(client_node)
    function_name = get_function_name()

    for dir in dirs:
        path = client.absolute_path(dir)

        def condition():

            try:
                mkdir(client, path)
                user.mark_last_operation_succeeded()
                return True
            except Exception as e:
                handle_exception(e, function_name)
                user.mark_last_operation_failed()
                return False

        client.perform(condition)


@when(parsers.parse('{user} creates directory and parents {paths} on {client_node}'))
@when(parsers.parse('{user} creates directory and parents {paths}\non {client_node}'))
def create_parents(user, paths, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    paths = list_parser(paths)
    function_name = get_function_name()

    for path in paths:
        dir_path = client.absolute_path(path)

        def condition():

            try:
                mkdir(client, dir_path, recursive=True)
                user.mark_last_operation_succeeded()
                return True
            except Exception as e:
                handle_exception(e, function_name)
                user.mark_last_operation_failed()
                return False

        client.perform(condition)


@when(parsers.parse('{user} deletes empty directories {dirs} on {client_node}'))
def delete_empty(user, dirs, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    dirs = list_parser(dirs)
    function_name = get_function_name()

    for dir in dirs:
        path = client.absolute_path(dir)

        def condition():

            try:
                rmdir(client, path)
                user.mark_last_operation_succeeded()
                return True
            except Exception as e:
                handle_exception(e, function_name)
                user.mark_last_operation_failed()
                return False

        client.perform(condition)


@when(parsers.parse('{user} deletes non-empty directories {dirs} on {client_node}'))
def delete_non_empty(user, dirs, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    dirs = list_parser(dirs)
    function_name = get_function_name()

    for dir in dirs:
        path = client.absolute_path(dir)

        def condition():
            try:
                rm(client, path, recursive=True, force=True)
                user.mark_last_operation_succeeded()
                return True
            except Exception as e:
                handle_exception(e, function_name)
                user.mark_last_operation_failed()
                return False

        client.perform(condition)


@when(parsers.parse('{user} deletes empty directory and parents {paths} on ' +
                    '{client_node}'))
def delete_parents(user, paths, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    paths = list_parser(paths)
    function_name = get_function_name()

    for path in paths:
        dir_path = client.absolute_path(path)

        def condition():

            try:
                rmdir(client, dir_path, recursive=True)
                user.mark_last_operation_succeeded()
                return True
            except Exception as e:
                handle_exception(e, function_name)
                user.mark_last_operation_failed()
                return False

        client.perform(condition)


@when(parsers.parse('{user} copies directory {dir1} to {dir2} on {client_node}'))
def copy_dir(user, dir1, dir2, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    src_path = client.absolute_path(dir1)
    dest_path = client.absolute_path(dir2)
    function_name = get_function_name()

    def condition():

        try:
            cp(client, src_path, dest_path, recursive=True)
            user.mark_last_operation_failed()
        except Exception as e:
            handle_exception(e, function_name)
            user.mark_last_operation_succeeded()

    client.perform(condition)


@when(parsers.parse('{user} can\'t list {dir} on {client_node}'))
@then(parsers.parse('{user} can\'t list {dir} on {client_node}'))
def cannot_list_dir(user, dir, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    path = client.absolute_path(dir)

    def condition():
        try:
            ls(client, path=path)
            return False
        except:
            return True

    assert client.perform(condition)


@when(parsers.parse('{user} can list {dir} on {client_node}'))
@then(parsers.parse('{user} can list {dir} on {client_node}'))
def list_dir(user, dir, client_node, context):
    user = context.get_user(user)
    client = user.get_client(client_node)
    path = client.absolute_path(dir)
    function_name = get_function_name()

    def condition():
        try:
            ls(client, path=path)
            return True
        except Exception as e:
            handle_exception(e, function_name)
            return False

    assert client.perform(condition)
