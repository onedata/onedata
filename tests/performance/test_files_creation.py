"""This module contains creating 10000 files performance tests of oneclient.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from pytest import fail

from tests.utils.docker_utils import run_cmd
from tests.performance.conftest import AbstractPerformanceTest
from tests.utils.performance_utils import (Result, generate_configs, performance)
from tests.utils.client_utils import user_home_dir, rm, mkdtemp

REPEATS = 1
SUCCESS_RATE = 100


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
                    'description': 'if true files will be created with no content',
                    'unit': 'boolean'
                }
            },
            'description': 'Testing file creation'
        },
        configs=generate_configs({
            'files_number': [10000],
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

        dir_path_directio = mkdtemp(client_directio,
                                    dir=client_directio.absolute_path('s1'))

        dir_path_proxy = mkdtemp(client_proxy,
                                 dir=client_proxy.absolute_path('s1'))
        dir_path_host = mkdtemp(client_proxy, dir=user_home_dir(user_proxy))

        # import pdb; pdb.set_trace()

        test_result1 = execute_file_creation_test(client_directio, user_directio,
                                                  files_number, empty_files,
                                                  dir_path_directio, 'direct IO')

        test_result2 = execute_file_creation_test(client_proxy, user_proxy,
                                                  files_number, empty_files,
                                                  dir_path_proxy, 'cluster-proxy')

        test_result3 = execute_file_creation_test(client_proxy, user_proxy,
                                                  files_number, empty_files,
                                                  dir_path_host, 'host system')

        rm(client_directio, dir_path_directio, recursive=True, force=True)
        rm(client_proxy, dir_path_proxy, recursive=True, force=True)
        rm(client_proxy, dir_path_host, recursive=True, force=True)

        return test_result1 + test_result2 + test_result3

################################################################################


def execute_file_creation_test(client, user, files_number, empty_files,
                               dir_path, description):
    cmd = prepare_command(files_number, empty_files, dir_path)
    result = run_cmd(user, client, [cmd])
    if result == 1:
        fail()

    return [
        Result('[{}] {} files creation'.format(description, files_number),
               1,
               '{} files creation using oneclient with {} content'
               ''.format(files_number, 'no' if empty_files else 'some'),
               'boolean')
    ]


def prepare_command(file_number, empty_files, dir_path):
    task, check = (('touch file_$i.txt', '')
                   if empty_files
                   else ('echo "asd" > file_$i.txt',
                         '&& (`cat file_$i.txt` == asd)'))
    return ('cd {dir} && '
            'for i in {{1..{range}}}; do {creation_task}; done && '
            'for i in {{1..{range}}}; do if [[ -f file_$i.txt {content_check} ]]; '
            'then continue; else false && break; fi; done').format(
            dir=dir_path,
            range=file_number,
            creation_task=task,
            content_check=check
    )
