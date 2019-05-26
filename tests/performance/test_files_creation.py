"""This module contains performance tests of oneclient,
testing creation of 10000 files.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import time
import os.path
from functools import partial

from tests.performance.conftest import AbstractPerformanceTest
from tests.utils.performance_utils import (Result, generate_configs, performance,
                                           flushed_print)
from tests.utils.client_utils import user_home_dir, rm, mkdtemp, truncate, write

REPEATS = 1
SUCCESS_RATE = 100
LOGGING_INTERVAL = 30
RPYC_TIMEOUT = 120

# value written to files at their creation
TEXT = "asd"


class TestFilesCreation(AbstractPerformanceTest):

    @performance(
        default_config={
            'repeats': REPEATS,
            'success_rate': SUCCESS_RATE,
            'parameters': {
                'files_number': {
                    'description': 'number of files',
                    'unit': 'int'
                },
                'empty_files': {
                    'description': 'if true, files will be '
                                   'created with no content',
                    'unit': 'boolean'
                }
            },
            'description': 'Testing file creation'
        },
        configs=generate_configs({
            'files_number': [5000],
            'empty_files': [True, False]
        }, 'FILE CREATION TEST -- '
           'Files number: {files_number} '
           'Empty files: {empty_files}'))
    def test_files_creation(self, context, clients, params):
        user_directio = "u1"
        user_proxy = "u2"
        client_directio = context.get_client(user_directio, 'client-directio')
        client_proxy = context.get_client(user_proxy, 'client-proxy')
        files_number = params['files_number']['value']
        empty_files = params['empty_files']['value']

        for client in (client_directio, client_proxy):
            conn = client.rpyc_connection
            conn._config['sync_request_timeout'] = RPYC_TIMEOUT

        dir_path_directio = mkdtemp(client_directio,
                                    dir=client_directio.absolute_path('s1'))

        dir_path_proxy = mkdtemp(client_proxy,
                                 dir=client_proxy.absolute_path('s1'))
        dir_path_host = mkdtemp(client_proxy, dir=user_home_dir(user_proxy))

        test_result1 = execute_file_creation_test(client_directio, files_number,
                                                  empty_files, dir_path_directio,
                                                  'direct IO')
        test_result2 = execute_file_creation_test(client_proxy, files_number,
                                                  empty_files, dir_path_proxy,
                                                  'cluster-proxy')
        test_result3 = execute_file_creation_test(client_proxy, files_number,
                                                  empty_files, dir_path_host,
                                                  'host system')

        # removal of entire directory tree can take several minutes, so as to
        # evade connection timeout while removing everything at once,
        # rm files one at time instead
        teardown_after_file_creation_test(client_directio, files_number,
                                          dir_path_directio)
        teardown_after_file_creation_test(client_proxy, files_number,
                                          dir_path_proxy)
        teardown_after_file_creation_test(client_proxy, files_number,
                                          dir_path_host)

        rm(client_directio, dir_path_directio, recursive=True, force=True)
        rm(client_proxy, dir_path_proxy, recursive=True, force=True)
        rm(client_proxy, dir_path_host, recursive=True, force=True)

        return test_result1 + test_result2 + test_result3

################################################################################


def execute_file_creation_test(client, files_number, empty_files,
                               dir_path, description):
    start = time.time()
    logging_time = start + LOGGING_INTERVAL

    fun = partial(truncate, size=0) if empty_files else partial(write,
                                                                text=TEXT)
    for i in xrange(files_number):
        fun(client, file_path=os.path.join(dir_path, 'file{}'.format(i)))
        if time.time() >= logging_time:
            flushed_print("\t\t\tCreated {}nth file".format(i))
            logging_time = time.time() + LOGGING_INTERVAL

    end = time.time()

    return [
        Result('[{}] {} files creation'.format(description, files_number),
               end - start,
               '{} files creation time using oneclient with {} content'
               ''.format(files_number, ('no' if empty_files else 'some')),
               'seconds')
    ]


def teardown_after_file_creation_test(client, files_number, dir_path):
    logging_time = time.time() + LOGGING_INTERVAL
    for i in xrange(files_number):
        rm(client, os.path.join(dir_path, 'file{}'.format(i)))
        if time.time() >= logging_time:
            flushed_print("\t\t\tDeleted {}nth file".format(i))
            logging_time = time.time() + LOGGING_INTERVAL
