import pytest
from pytest_bdd import (given, when, then)
from pytest_bdd import parsers

from environment import docker, env


@given(parsers.parse('we are in space {space}'))
def goto_space(space, client_id):
    path = docker.exec_(container=client_id, command="pwd", output=True)
    if not path.endswith("spaces/" + space):
        docker.exec_(container=client_id, command="cd ~/onedata/spaces/" + space)


@when(parsers.parse('user creates directories {dirs}'))
def create(dirs, client_id):
    dirs = list_parser(dirs)
    for dir in dirs:
        docker.exec_(container=client_id, command=["mkdir", dir])
    print("Create")


@when(parsers.parse('user creates directory and parents {paths}'))
def create_parents(paths, client_id):
    paths = list_parser(paths)
    for path in paths:
        docker.exec_(container=client_id, command=["mkdir", "-p", path])
    print("Create parents")


@when(parsers.parse('user renames {dir1} to {dir2}'))
def rename(dir1, dir2, client_id):
    docker.exec_(container=client_id, command=["mv", dir1, dir2])
    print("Rename")


@when(parsers.parse('user deletes empty directory {dir}'))
def delete_empty(dir, client_id):
    docker.exec_(container=client_id, command=["rmdir", dir])
    print("Delete")


@when(parsers.parse('user deletes non-empty directory {dir}'))
def delete_non_empty(dir, client_id):
    docker.exec_(container=client_id, command=["rm -r", dir])
    print("Delete non-empty")


@when(parsers.parse('user deletes empty directory and parents {path}'))
def delete_parents(path, client_id):
    docker.exec_(container=client_id, command=["rmdir -p", path])
    print("Delete parents")


@then(parsers.parse('{dirs} are in ls {path}'))
def ls_present(dirs, path, client_id):
    print "PATH: " + path
    ls_dirs = docker.exec_(container=client_id, command=["ls", path], output=True).split()
    print ls_dirs
    dirs = list_parser(dirs)
    for dir in dirs:
        if not dir in ls_dirs:
            return False
    return True


@then(parsers.parse('{dirs} are not in ls {path}'))
def ls_absent(dirs, path, client_id):
    ls_dirs = docker.exec_(container=client_id, command=["ls", path], output=True).split()
    dirs = list_parser(dirs)
    for dir in dirs:
        if dir in ls_dirs:
            return False
    return True

#####################################################################

def list_parser(list):
    # return list.strip("[]").split(", ")
    return [el.strip() for el in list.strip("[]").split(',')]
