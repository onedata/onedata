"""Steps directly interacting with docker.
"""

import time
import subprocess
import os.path

from tests.gui.utils.generic import parse_seq, suppress

from pytest_bdd import given, when, then, parsers


__author__ = "Jakub Liput, Bartosz WAlkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


PROVIDER_CONTAINER_NAME = 'node1.oneprovider.1496901721.dev'
MOUNT_POINT = '/mnt/st1/'


def _docker_cp(src_path, dst_path, container):
    cmd = ["docker", "cp"]
    cmd.extend([src_path, "{0}:{1}".format(container, dst_path)])
    subprocess.check_call(cmd)


@when(parsers.parse('user of {browser_id} copies {src_path} '
                    'to provider\'s storage mount point'))
@then(parsers.parse('user of {browser_id} copies {src_path} '
                    'to provider\'s storage mount point'))
def wt_user_copies_files_to_docker(browser_id, src_path, tmpdir):
    src_path = os.path.join(str(tmpdir), browser_id, src_path)
    dst_path = os.path.join(MOUNT_POINT, os.path.basename(src_path))
    _docker_cp(src_path, dst_path, PROVIDER_CONTAINER_NAME)


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
