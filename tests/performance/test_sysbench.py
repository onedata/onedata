"""
This module contains performance tests of oneclient using sysbench benchmark.
"""
__author__ = "Jakub Kudzia"
__copyright__ = """(C) 2016 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""

import re

from tests.utils.docker_utils import run_cmd
from tests.performance.conftest import AbstractPerformanceTest, performance
from tests.utils.performance_utils import (Result, generate_configs, temp_dir,
                                           get_home_dir, delete_file,
                                           performance)

# TODO functions used in cucumber, acceptance and performance tests should be moved to common files
# TODO higher in files hierarchy
REPEATS = 1
SUCCESS_RATE = 95
DD_OUTPUT_REGEX = r'.*\s+s, (\d+\.?\d+?) (\w+/s)'
DD_OUTPUT_PATTERN = re.compile(DD_OUTPUT_REGEX)
SYSBENCH_OUTPUT_REGEX = r'Total transferred (\d+(\.\d+))?\w+\s+\((\d+(\.\d+)?)(\w+/\w+)\)\s+(\d+(\.\d+)?)\s+(\w+/\w+)'
SYSBENCH_OUTPUT_PATTERN = re.compile(SYSBENCH_OUTPUT_REGEX, re.MULTILINE)


class TestSysbench(AbstractPerformanceTest):

    @performance(
            default_config={
                'repeats': REPEATS,
                'success_rate': SUCCESS_RATE,
                'parameters': {
                    'files_number': {
                        'description': "Number of files",
                        'unit': ""
                    },
                    'threads_number': {
                        'description': "Number of threads",
                        'unit': ""
                    },
                    'mode': {
                        'description': "Modes",
                        'unit': ""
                    },
                    'total_size': {
                        'description': "Total size",
                        'unit': "MB"
                    }
                },
                'description': 'Testing file system using sysbench'
            },
            configs=generate_configs({
                'files_number': [10, 100],# 1000],
                'threads_number': [1, 16],
                'total_size': [100],# 1000],
                'mode': ["rndrw", "rndrd", "rndwr", "seqwr", "seqrd"]
            }, 'SYSBENCH TEST -- '
               'Files number: {files_number} '
               'Threads number: {threads_number} '
               'Total Size: {total_size} '
               'Mode: {mode}')

    )
    def test_sysbench(self, clients, params):
        client_directio = clients['client_directio']
        client_proxy = clients['client_proxy']
        threads_number = params['threads_number']['value']
        files_number = params['files_number']['value']
        total_size = params['total_size']['value']
        mode = params['mode']['value']

        dir_path_directio = temp_dir(client_directio, client_directio.mount_path)
        dir_path_proxy = temp_dir(client_proxy, client_proxy.mount_path)
        dir_path_host = temp_dir(client_proxy, get_home_dir(client_proxy))

        test_result1 = execute_sysbench_test(client_directio, threads_number,
                                             total_size, files_number, mode,
                                             dir_path_directio, "direct IO")

        test_result2 = execute_sysbench_test(client_proxy, threads_number,
                                             total_size, files_number, mode,
                                             dir_path_proxy, "cluster-proxy")

        test_result3 = execute_sysbench_test(client_proxy, threads_number,
                                             total_size, files_number, mode,
                                             dir_path_host, "host system")

        delete_file(client_directio, dir_path_directio)
        delete_file(client_proxy, dir_path_proxy)
        delete_file(client_proxy, dir_path_host)

        return test_result1 + test_result2 + test_result3

################################################################################


def execute_sysbench_test(client, threads_number, total_size, files_number,
                          mode, dir_path, description):
    out = parse_sysbench_output(sysbench_tests(
            threads_number, total_size, files_number, mode, client, dir_path))
    return [
        Result("transfer_{}".format(description),
               out['transfer'],
                   "Transfer velocity in case of {}".format(description),
               out['transfer_unit']),
        Result("requests_{}".format(description),
               out['requests_velocity'],
                   "Requests per second in onedata {}".format(description),
               out['requests_velocity_unit']),
    ]


def sysbench_tests(threads_number, total_size, file_number, mode, client, dir):
    run_sysbench_prepare(threads_number, total_size, file_number, mode, client, dir)
    output = run_sysbench(threads_number, total_size, file_number, mode, client, dir)
    run_sysbench_cleanup(threads_number, total_size, file_number, mode, client, dir)
    return output


def run_sysbench_prepare(threads_number, total_size, file_number, mode, client, dir):
    sysbench(threads_number, total_size, file_number, mode, "prepare", client, dir)


def run_sysbench_cleanup(threads_number, total_size, file_number, mode, client, dir):
    sysbench(threads_number, total_size, file_number, mode, "cleanup", client, dir)


def run_sysbench(threads_number, total_size, file_number, mode, client, dir):
    return sysbench(threads_number, total_size, file_number, mode, "run", client, dir)


def sysbench(threads_number, total_size, file_number, mode, type, client, dir):
    cmd = sysbench_command(threads_number, total_size, file_number, mode, type, dir)
    return run_cmd(client.user, client, [cmd], output=True)


def sysbench_command(threads_number, total_size, file_number, mode, type, dir):

    cmd = ('cd {dir} && '
           'sysbench --num-threads={threads_number} --test=fileio '
           '--file-total-size={total_size}M --file-num={file_number} '
           '--file-test-mode={mode} {type}').format(
            dir=dir,
            threads_number=threads_number,
            total_size=total_size,
            file_number=file_number,
            mode=mode,
            type=type)

    return cmd


def parse_sysbench_output(output):
    m = re.search(SYSBENCH_OUTPUT_PATTERN, output)
    transfer = float(m.group(3))
    transfer_unit = m.group(5)
    requests_velocity = float(m.group(6))
    requests_velocity_unit = m.group(8)

    return {'transfer': transfer,
            'transfer_unit': transfer_unit,
            'requests_velocity': requests_velocity,
            'requests_velocity_unit': requests_velocity_unit}
