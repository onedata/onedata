"""
This module contains performance tests of oneclient using sysbench benchmark.
"""
__author__ = "Jakub Kudzia"
__copyright__ = """(C) 2016 ACK CYFRONET AGH,
This software is released under the MIT license cited in 'LICENSE.txt'."""


from tests.performance.conftest import TestPerformance, performance
from environment import docker, env
import pytest
from tests.performance.utils import TestResult, generate_configs
import random
from tests.cucumber.scenarios.steps.common import Client, run_cmd
import os
import re
import subprocess

# TODO functions used in cucumber, acceptance and performance tests should be moved to common files
# TODO higher in files hierarchy
REPEATS = 100
SUCCESS_RATE = 95
DD_OUTPUT_REGEX = r'.*\s+s, (\d+\.?\d+?) (\w+/s)'
DD_OUTPUT_PATTERN = re.compile(DD_OUTPUT_REGEX)
SYSBENCH_OUTPUT_REGEX = r'Total transferred \d+.?\d+?\w+\s+\((\d+.?\d+)(\w+/\w+)\)\s+(\d+.?\d+?)\s+(\w+/\w+)'
SYSBENCH_OUTPUT_PATTERN = re.compile(SYSBENCH_OUTPUT_REGEX, re.MULTILINE)


class TestSysbench(TestPerformance):

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
                'description': 'Sysbench test'
            },
            configs=generate_configs({
                'files_number': [10],#, 100, 1000],
                'threads_number': [1],#,, 16],
                'total_size': [10],#, 100, 1000],
                'mode': ["rndrw"],#, "rndrd", "rndwr", "seqwr", "seqrd"]
            }, 'SYSBENCH TEST -- '
               'Files number: {files_number} '
               'Threads number: {threads_number} '
               'Total Size: {total_size} '
               'Mode: {mode}')

    )
    def test_sysbench(self, clients, params):

        client_name, client = clients.items()[0]

        threads_number = params['threads_number']['value']
        files_number = params['files_number']['value']
        total_size = params['total_size']['value']
        mode = params['mode']['value']

        dir_path = temp_dir(client, client.mount_path)
        dir_path_host = temp_dir(client, get_home_dir(client))

        output = sysbench_tests(threads_number, total_size, files_number, mode,
                                client, dir_path)

        output_host = sysbench_tests(threads_number, total_size, files_number,
                                     mode, client, dir_path_host)

        (
            (transfer, transfer_unit),
            (requests_velocity, requests_velocity_unit)
        ) = parse_sysbench_output(output)

        (
            (transfer_host, transfer_unit_host),
            (requests_velocity_host, requests_velocity_unit_host)
        ) = parse_sysbench_output(output_host)

        return [
            TestResult("transfer", transfer, "Transfer velocity in onedata",
                       transfer_unit),
            TestResult("requests", requests_velocity,
                       "Requests per second in onedata",
                       requests_velocity_unit),
            TestResult("transfer_host", transfer_host,
                       "Transfer velocity on host", transfer_unit_host),
            TestResult("requests_host", requests_velocity_host,
                       "Requests per second on host",
                       requests_velocity_unit_host)
        ]


################################################################################

def convert_size(value, prefix, convert_to_prefix):
    convert_to_prefix = convert_to_prefix.upper()
    si_powers_prefixes = ['kB', 'MB', 'GB', 'TB']
    si_powers_values = [1000 ** p for p in range(1, 5)]
    si_powers = dict(zip(si_powers_prefixes, si_powers_values))
    powers_prefixes = ['K', 'M', 'G', 'T']
    powers_values = [1024 ** p for p in range(1, 5)]
    powers = dict(zip(powers_prefixes, powers_values))

    if is_SI_prefix(prefix):
        factor = float(si_powers[prefix]) / powers[convert_to_prefix]
    else:
        factor = float(powers[prefix]) / powers[convert_to_prefix]

    return value * factor


def is_SI_prefix(prefix):
    return prefix.endswith('B')


def SI_prefix_to_default(prefix):
    return prefix.upper().strip("B")


def create_file(client, path):
    run_cmd('u1', client, "touch " + path)


def temp_file(client, path):
    cmd = '''import tempfile
handle, file_path = tempfile.mkstemp(dir="{dir}")
print file_path'''

    cmd = cmd.format(dir=path)
    cmd = ["python -c '{command}'".format(command=cmd)]

    return run_cmd(client.user, client, cmd, output=True).strip()


def temp_dir(client, path):
    cmd = '''import tempfile
print tempfile.mkdtemp(dir="{dir}")'''

    cmd = cmd.format(dir=path)
    cmd = ["python -c '{command}'".format(command=cmd)]
    return run_cmd(client.user, client, cmd, output=True).strip()


def delete_file(client, path):
    run_cmd('u1', client, "rm -rf " + path)


def sysbench_tests(threads_number, total_size, file_number, mode, client, dir):
    run_sysbench_prepare(threads_number, total_size, file_number, mode, client,
                         dir)
    output = run_sysbench(threads_number, total_size, file_number, mode, client,
                          dir)
    run_sysbench_cleanup(threads_number, total_size, file_number, mode, client,
                         dir)
    return output


def run_sysbench_prepare(threads_number, total_size, file_number, mode, client,
                         dir):
    sysbench(threads_number, total_size, file_number, mode, "prepare", client,
             dir)


def run_sysbench_cleanup(threads_number, total_size, file_number, mode, client,
                         dir):
    sysbench(threads_number, total_size, file_number, mode, "cleanup", client,
             dir)


def run_sysbench(threads_number, total_size, file_number, mode, client, dir):
    return sysbench(threads_number, total_size, file_number, mode, "run",
                    client, dir)


def sysbench(threads_number, total_size, file_number, mode, type, client, dir):
    cmd = sysbench_command(threads_number, total_size, file_number, mode, type,
                           dir)
    return run_cmd(client.user, client, [cmd], output=True)


def sysbench_command(threads_number, total_size, file_number, mode, type, dir):

    cmd = "cd {dir} && " \
          "sysbench --num-threads={threads_number} --test=fileio " \
          "--file-total-size={total_size}M --file-num={file_number} " \
          "--file-test-mode={mode} {type}".format(
            dir=dir,
            threads_number=threads_number,
            total_size=total_size,
            file_number=file_number,
            mode=mode,
            type=type)

    return cmd


def parse_sysbench_output(output):
    m = re.search(SYSBENCH_OUTPUT_PATTERN, output)
    transfer = float(m.group(1))
    transfer_unit = m.group(2)
    requests_velocity = float(m.group(3))
    requests_velocity_unit = m.group(4)

    return ((transfer, transfer_unit), (requests_velocity, requests_velocity_unit))


def get_home_dir(client):
    return os.path.join("/home", client.user)
