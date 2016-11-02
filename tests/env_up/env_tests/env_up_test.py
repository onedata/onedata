"""This module contains acceptance test of env_up script.
"""
__author__ = "Lukasz Opiola, Jakub Kudzia"
__copyright__ = "Copyright (C) 2015 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from tests.utils.utils import run_env_up_script
from tests.utils.path_utils import config_file, get_file_name, make_logdir
from test_utils import *

from environment import docker


class TestEnvUp:
    @classmethod
    # Run the env_up.py script, capture and parse the output
    def setup_class(cls):
        logdir = make_logdir(ENV_UP_LOGDIR, get_file_name(__file__))
        result = run_env_up_script("env_up.py",
                                   config=config_file('env.json'),
                                   logdir=logdir, skip=False)
        cls.result = result

    @classmethod
    # Clean up removing all dockers created in the test
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # Test if the env_up.py script works as expected.
    def test_env_up(self):
        env = self.result
        # Check if number of started dockers is correct
        # The number should be:
        # 1 dns node

        # 4 op_worker nodes
        # 2 cluster_manager nodes for op_workers
        # 2 DB nodes for op_workers

        # 1 OZ nodes
        # 1 cluster_manager nodes for oz_worker
        # 1 DB node for oz_worker

        # 2 appmock nodes

        # 2 client nodes
        # ------------
        # 16 nodes
        assert 16 == len(env['docker_ids'])

        check_zone_up(env, 1)
        check_cluster_manager_up(env, 3)
        check_provider_worker_up(env, 4)
        check_appmock_up(env, 2)
        check_client_up(env, 2)
