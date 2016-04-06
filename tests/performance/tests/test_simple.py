from tests.performance.conftest import TestPerformance, performance
from environment import docker, env
import pytest

# TODO functions used in cucumber and acceptance tests should be moved to utils

REPEATS = 1
SUCCESS_RATE = 95


class TestSimple(TestPerformance):

    @performance(
            default_config={
                'repeats': REPEATS,
                'success_rate': SUCCESS_RATE,
                'parameters': {
                    'sizes': [1024, 102400, 1048576, 5242880,
                              52428800],
                    'block_sizes': [1, 4, 128, 1024]
                },
                'description': 'Simple performance test1'
            },
            configs={
                'name1': {
                    'description': 'Description of name1 config',
                }
            }
    )
    def test1(self, clients, params):
        print "TEST clients: ", clients
        print "TEST CONFIG: ", params
        print "FILE: ", __name__

    @performance(
            default_config={'repeats': REPEATS,
                            'success_rate': SUCCESS_RATE,
                            'parameters': {
                                'sizes': [1024, 102400, 1048576, 5242880,
                                          52428800],
                                'block_sizes': [1, 4, 128, 1024]
                            },
                            'description': 'Simple performance test2'
                            },
            configs={
                'name1': {
                    'description': 'Description of name2 config',
                }
            }
    )
    def test2(self, clients, params):
        print "TEST clients: ", clients
        print "TEST CONFIG: ", params

###################### FIXTURES  ######################
