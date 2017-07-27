"""This module contains gherkin steps to run acceptance tests featuring
interaction with docker in onezone web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = ("This software is released under the MIT license cited in "
               "LICENSE.txt")


import time
import subprocess
import os.path

from tests.gui.utils.generic import parse_seq, suppress

from pytest_bdd import given, when, then, parsers


PROVIDER_CONTAINER_NAME = 'oneprovider-1'
MOUNT_POINT = '/volumes/storage'


def _docker_cp(tmpdir, browser_id, src_path, dst_path=None):
    src_path = os.path.join(str(tmpdir), browser_id, src_path)
    if dst_path:
        cmd = ['docker', 'exec', PROVIDER_CONTAINER_NAME,
               'mkdir', '-p', dst_path]
        subprocess.call(cmd)
    else:
        dst_path = MOUNT_POINT

    cmd = ["docker", "cp", src_path, "{0}:{1}".format(PROVIDER_CONTAINER_NAME,
                                                      dst_path)]
    subprocess.check_call(cmd)


def _docker_rm(path):
    cmd = ['docker', 'exec', PROVIDER_CONTAINER_NAME, 'rm', '-rf', path]
    subprocess.check_call(cmd)


@when(parsers.parse('user of {browser_id} copies {src_path} '
                    'to provider\'s storage mount point'))
@then(parsers.parse('user of {browser_id} copies {src_path} '
                    'to provider\'s storage mount point'))
def wt_cp_files_to_storage_mount_point(browser_id, src_path, tmpdir):
    _docker_cp(tmpdir, browser_id, src_path)


@when(parsers.parse('user of {browser_id} copies {src_path} '
                    'to the root directory of "{space}" space'))
@then(parsers.parse('user of {browser_id} copies {src_path} '
                    'to the root directory of "{space}" space'))
def wt_cp_files_to_space_root_dir(browser_id, src_path, space,
                                  tmpdir, tmp_memory):
    _docker_cp(tmpdir, browser_id, src_path,
               os.path.join(MOUNT_POINT, tmp_memory['spaces'][space]))


@when(parsers.parse('user of {browser_id} copies {src_path} '
                    'to {dst_path} directory of "{space}" space'))
@then(parsers.parse('user of {browser_id} copies {src_path} '
                    'to {dst_path} directory of "{space}" space'))
def wt_cp_files_to_dst_path_in_space(browser_id, src_path, dst_path,
                                     space, tmpdir, tmp_memory):
    _docker_cp(tmpdir, browser_id, src_path,
               os.path.join(MOUNT_POINT, tmp_memory['spaces'][space],
                            dst_path))


@when(parsers.parse('user of {browser_id} removes {src_path} '
                    'from provider\'s storage mount point'))
@then(parsers.parse('user of {browser_id} removes {src_path} '
                    'from provider\'s storage mount point'))
def wt_rm_files_to_storage_mount_point(src_path):
    _docker_rm(os.path.join(MOUNT_POINT, src_path))


@when(parsers.parse('user of {browser_id} removes {src_path} '
                    'from the root directory of "{space}" space'))
@then(parsers.parse('user of {browser_id} removes {src_path} '
                    'from the root directory of "{space}" space'))
def wt_rm_files_to_space_root_dir(src_path, space, tmp_memory):
    _docker_rm(os.path.join(MOUNT_POINT, tmp_memory['spaces'][space],
                            src_path))


@given(parsers.parse('there is no working provider named {provider_list}'))
@given(parsers.parse('there are no working providers named {provider_list}'))
def kill_providers(persistent_environment, provider_list):
    kill_cmd = ['docker', 'kill']
    inspect_cmd = ['docker', 'inspect', '-f', '{{.State.Running}}']
    for provider in parse_seq(provider_list):
        for node in persistent_environment["op_worker_nodes"]:
            if provider in node:
                container_name = node.split('@')[1]
                subprocess.call(kill_cmd + [container_name])
                for _ in xrange(10):
                    is_alive = subprocess.Popen(inspect_cmd + [container_name],
                                                stdout=subprocess.PIPE)
                    with suppress(Exception):
                        if is_alive.communicate()[0] == 'false\n':
                            break
                    time.sleep(1)
                else:
                    raise RuntimeError('container {} still alive, while it '
                                       'should not be'.format(container_name))
