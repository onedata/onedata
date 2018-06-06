"""This module contains performance tests of oneclient using sysbench benchmark.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from tests.utils.docker_utils import run_cmd
from tests.performance.conftest import AbstractPerformanceTest
from tests.utils.performance_utils import generate_configs, performance
from tests.utils.client_utils import rm, mkdtemp

REPEATS = 1
SUCCESS_RATE = 100

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
                'threads': {
                    'description': "Number of threads to use",
                    'unit': ""
                },
                'mode': {
                    'description': "Modes",
                    'unit': ""
                },
                'total_size': {
                    'description': "Total size",
                    'unit': "MB"
                },
                'validate': {
                    'description': "Perform validation of test results where possible",
                    'unit': ""
                },
                'events': {
                    'description': "Limit for total number of events.",
                    'unit': ""
                },
                'report_interval': {
                    'description': "Periodically report intermediate statistics with a specified interval in seconds. "
                                   "Note that statistics produced by this option is per-interval rather than cumulative. "
                                   "0 disables intermediate reports",
                    'unit': "s"
                },
                'time': {
                    'description': "Limit for total execution time in seconds.",
                    'unit': "s"
                },
                'file_block_size': {
                    'description': "Block size to use in all I/O operations",
                    'unit': "KB"
                }
            },
            'description': 'Testing file system using sysbench'
        },
        configs=generate_configs({
            'files_number': [1],
            'threads': [1],
            'total_size': [2048],
            'mode': ["rndrd", "rndrw", "rndwr", "seqrd", "seqwr"],
            'validate': ["on"],
            'events': [1000000],
            'report_interval': [15],
            'time': [0],
            'file_block_size': [4]
        }, 'SYSBENCH TEST -- '
           'Files number: {files_number} '
           'Threads number: {threads} '
           'Total Size: {total_size} '
           'Mode: {mode}'))
    def test_sysbench(self, context, clients, params):
        user_directio = "u1"
        user_proxy = "u2"
        client_directio = context.get_client(user_directio, "client-directio")
        client_proxy = context.get_client(user_proxy, "client-proxy")
        threads = params['threads']['value']
        files_number = params['files_number']['value']
        total_size = params['total_size']['value']
        mode = params['mode']['value']
        validate = params['validate']['value']
        events = params['events']['value']
        report_interval = params['report_interval']['value']
        time = params['time']['value']
        file_block_size = params['file_block_size']['value']

        dir_path_directio = mkdtemp(client_directio,
                                    dir=client_directio.absolute_path("s1"))

        dir_path_proxy = mkdtemp(client_proxy,
                                 dir=client_proxy.absolute_path("s1"))


        print("\n################################## DIRECT-IO client (%s) ##################################\n"%(mode))

        execute_sysbench_test(client_directio, user_directio, threads,
                              total_size, files_number, mode, validate,
                              events, report_interval, time,
                              file_block_size, dir_path_directio)

        print("\n################################## PROXY-IO client (%s) ##################################\n"%(mode))

        execute_sysbench_test(client_proxy, user_proxy, threads,
                              total_size, files_number, mode, validate,
                              events, report_interval, time,
                              file_block_size, dir_path_proxy)



        rm(client_directio, dir_path_directio, recursive=True, force=True)
        rm(client_proxy, dir_path_proxy, recursive=True, force=True)


################################################################################


def execute_sysbench_test(client, user, threads, total_size, files_number,
                          mode, validate, events, report_interval,
                          time, file_block_size, dir_path):
    sysbench_tests(threads, total_size, files_number, mode, validate,
                   events, report_interval, time, file_block_size,
                   client, user, dir_path)


def sysbench_tests(threads, total_size, file_number, mode, validate,
                   events, report_interval, time, file_block_size, client, user, dir):
    run_sysbench_prepare(threads, total_size, file_number, mode, validate,
                         events, report_interval, time, file_block_size, client, user, dir)
    code = run_sysbench(threads, total_size, file_number, mode, validate,
                          events, report_interval, time, file_block_size, client, user, dir)
    assert(code == 0)
    run_sysbench_cleanup(threads, total_size, file_number, mode, validate,
                         events, report_interval, time, file_block_size, client, user, dir)


def run_sysbench_prepare(threads, total_size, file_number, mode, validate,
                         events, report_interval, time, file_block_size,
                         client, user, dir):
    print("=== Preparing sysbench test files...")
    return sysbench(threads, total_size, file_number, mode, validate,
             events, report_interval, time, file_block_size,
             "prepare", client, user, dir, output=True)


def run_sysbench_cleanup(threads, total_size, file_number, mode,
                         validate, events, report_interval, time,
                         file_block_size, client, user, dir):
    print("=== Removing sysbench test files...")
    return sysbench(threads, total_size, file_number, mode, validate,
             events, report_interval, time, file_block_size,
             "cleanup", client, user, dir, output=True)


def run_sysbench(threads, total_size, file_number, mode, validate,
                 events, report_interval, time, file_block_size,
                 client, user, dir):
    return sysbench(threads, total_size, file_number, mode, validate,
                    events, report_interval, time, file_block_size,
                    "run", client, user, dir)


def sysbench(threads, total_size, file_number, mode, validate,
             events, report_interval, time, file_block_size, type,
             client, user, dir, output=False):
    cmd = sysbench_command(threads, total_size, file_number, mode,
                           validate, events, report_interval, time,
                           file_block_size, type, dir)
    return run_cmd(user, client, [cmd], output=output)


def sysbench_command(threads, total_size, file_number, mode, validate,
                     events, report_interval, time, file_block_size,
                     type, dir):

    cmd = ('cd {dir} && '
           'sysbench '
           '--threads={threads} '
           '--file-total-size={total_size}M '
           '--file-num={file_number} '
           '--file-test-mode={mode} '
           '--validate={validate} '
           '--events={events} '
           '--report-interval={report_interval} '
           '--time={time} '
           '--file-block-size={file_block_size}K '
           'fileio '
           '{type}').format(
            dir=dir,
            threads=threads,
            total_size=total_size,
            file_number=file_number,
            mode=mode,
            validate=validate,
            events=events,
            report_interval=report_interval,
            time=time,
            file_block_size=file_block_size,
            type=type)

    return cmd

