import os
import sys

from tests.test_common import *
import subprocess
import ast
import pytest
import pprint

from environment import docker, env


@pytest.fixture(scope="module", params=
    [os.path.join(get_test_data_dir(__file__), "env.json")]
    #params=get_json_files(get_test_data_dir(__file__))
)
def environment(request):
    logdir = make_logdir(acceptance_logdir, get_test_name(__file__))
    env = run_env_up_script("env_up.py", [
        '-l', logdir, request.param
    ])

    def fin():
        docker.remove(request.environment['docker_ids'], force=True, volumes=True)
    request.addfinalizer(fin)

    return env


def test_dd(environment):
    pprint.pprint(environment)


def mount_oneclient(env):
    pass
