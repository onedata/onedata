"""This file contains utility functions for profiling of op-worker during
acceptance tests of onedata.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import UTILS_DIR
from tests.utils.string_utils import replace
from environment import docker

import subprocess
import os


FPROF_DATA_DIR = '/root/bin/node'
FPROF_DATA_FILE = 'fprof.analysis'
FPROF_DATA_PATH = os.path.join(FPROF_DATA_DIR, FPROF_DATA_FILE)


def start_fprof(op_node):
    subprocess.check_output([os.path.join(UTILS_DIR, "start_fprof.escript"),
                            op_node])


def stop_fprof(op_node):
    subprocess.check_output([os.path.join(UTILS_DIR, 'stop_fprof.escript'),
                             op_node])


def copy_fprof_data(worker_docker, dest_dir):
    docker.cp(worker_docker, FPROF_DATA_PATH, dest_dir)


def convert_fprof_data(file_path):
    dir_name = os.path.dirname(file_path)
    escape_erlang_pids(file_path)
    output = os.path.join(dir_name, 'callgrind.out')
    subprocess.check_output(['erlgrind', file_path, output])


def escape_erlang_pids(file):
    """Escapes all erlang pids in file by surrounding them with quotation marks
    """
    pid_regex = r'(?=[^"])(<\d+\.\d+\.\d+>)(?=[^"])'

    def repl(match):
        return '"{}"'.format(match.group(1))

    replace(file, pid_regex, repl)