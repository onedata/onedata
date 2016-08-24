"""Module implements pytest-bdd steps for starting profiling op-worker during
bdd tests.
"""
import os

from tests import PROFILING_LOGDIR
from tests.utils.path_utils import make_logdir
from tests.utils.utils import get_matching_op_erl_node

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.acceptance_utils import *
from pytest_bdd import when
from tests.utils.profiling_utils import start_fprof, stop_fprof, copy_fprof_data, \
    FPROF_DATA_FILE, convert_fprof_data


@when(parsers.parse('profiling is started on {op_worker}'))
@then(parsers.parse('profiling is started on {op_worker}'))
def start_profiling(op_worker, context, onedata_environment):
    start_fprof(get_matching_op_erl_node(op_worker, onedata_environment))


@when(parsers.parse('profiling is stopped on {op_worker}'))
@then(parsers.parse('profiling is stopped on {op_worker}'))
def stop_profiling(op_worker, context, onedata_environment, request, providers):
    logdir = make_logdir(PROFILING_LOGDIR, request.function.func_name)
    erl_node = get_matching_op_erl_node(op_worker, onedata_environment)
    docker_name = erl_node.split('@')[1]
    stop_fprof(erl_node)
    copy_fprof_data(docker_name, logdir)
    convert_fprof_data(os.path.join(logdir, FPROF_DATA_FILE))