"""This module contains performance tests of dd operation in oneclient.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.performance.conftest import AbstractPerformanceTest
from tests.utils.performance_utils import (Result, generate_configs, performance)
from tests.utils.client_utils import mkstemp, rm, user_home_dir, dd

import os
import re

REPEATS = 3
SUCCESS_RATE = 95
DD_OUTPUT_REGEX = r'.*\s+s, (\d+\.?\d+?) (\w+/s)'
DD_OUTPUT_PATTERN = re.compile(DD_OUTPUT_REGEX)
SYSBENCH_OUTPUT_REGEX = r'Total transferred \d+.?\d+?\w+\s+\((\d+.?\d+)(\w+/\w+)\)\s+(\d+.?\d+?)\s+(\w+/\w+)'
SYSBENCH_OUTPUT_PATTERN = re.compile(SYSBENCH_OUTPUT_REGEX, re.MULTILINE)


class Testdd(AbstractPerformanceTest):

    @performance(
        default_config={
            'repeats': REPEATS,
            'success_rate': SUCCESS_RATE,
            'parameters': {
                'size': {'description': "size", 'unit': "kB"},
                'block_size': {'description': "size of block", 'unit': "kB"}
            },
            'description': 'Test of dd throughput'
        },
        configs=generate_configs({
            'block_size': [1, 4, 128, 1024],
            'size': [1024, 10240]#, 1048576, 10485760]
        }, "DD TEST -- block size: {block_size} size: {size}"))
    def test_dd(self, context, clients, params):

        size = params['size']['value']
        size_unit = params['size']['unit']
        block_size = params['block_size']['value']
        block_size_unit = params['block_size']['unit']
        user_directio = "u1"
        user_proxy = "u2"

        client_directio = context.get_client(user_directio, "client-directio")
        client_proxy = context.get_client(user_proxy, "client-proxy")

        test_file_directio = mkstemp(client_directio,
                                     dir=client_directio.absolute_path("s1"))

        test_file_proxy = mkstemp(client_proxy,
                                  dir=client_proxy.absolute_path("s1"))
        test_file_host = mkstemp(client_proxy, dir=user_home_dir(user_proxy))

        test_result1 = execute_dd_test(client_directio, user_directio,
                                       test_file_directio, block_size,
                                       block_size_unit, size, size_unit,
                                       "direct IO")

        test_result2 = execute_dd_test(client_proxy, user_proxy,
                                       test_file_proxy, block_size,
                                       block_size_unit, size, size_unit,
                                       "cluster-proxy")

        test_result3 = execute_dd_test(client_proxy, user_proxy, test_file_host,
                                       block_size, block_size_unit, size,
                                       size_unit, "host system")

        rm(client_directio, test_file_directio, recursive=True, force=True)
        rm(client_proxy, test_file_proxy, recursive=True, force=True)
        rm(client_proxy, test_file_host, recursive=True, force=True)

        return test_result1 + test_result2 + test_result3


################################################################################

def execute_dd_test(client, user, test_file, block_size, block_size_unit, size,
                    size_unit, description):

    dev_zero = os.path.join('/dev', 'zero')

    write_throughput = parse_dd_output(do_dd(client, user, dev_zero, test_file,
                                             block_size, block_size_unit, size,
                                             size_unit))

    read_throughput = parse_dd_output(do_dd(client, user, test_file, dev_zero,
                                            block_size, block_size_unit, size,
                                            size_unit))

    return [
        Result('write_throughput_{}'.format(description),
               write_throughput,
               "Throughput of write operation in case of {}".format(description),
                "MB/s"),
        Result('read_throughput_{}'.format(description),
               read_throughput,
               "Throughput of read operation in case of {}".format(description),
               "MB/s")
    ]


def do_dd(client, user, input, output, block_size, block_size_unit, size, size_unit):
    block_size_unit = SI_prefix_to_default(block_size_unit)
    size_unit = SI_prefix_to_default(size_unit)
    size = convert_size(size, size_unit, 'k')
    block_size = convert_size(block_size, block_size_unit, 'k')
    count = size / block_size

    return dd(client, int(block_size), int(count), output, unit='k',
              output=True, error=True, input_file=input, user=user)


def parse_dd_output(dd_output):
    dd_output = dd_output.split("\n")[-1].strip()
    m = re.match(DD_OUTPUT_PATTERN, dd_output)
    value = float(m.group(1))
    unit = m.group(2)
    size_unit = unit.split('/')[0]
    return convert_size(value, size_unit, 'M')


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
