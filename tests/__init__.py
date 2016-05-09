"""This file contains definitions of constants used in tests.
It also append usefule modules to sys.path to make them available
in tests.
"""
import os
import sys

__author__ = "Jakub Kudzia"

current_dir = os.path.dirname(os.path.realpath(__file__))

# # Define constants for use in tests
# directories
PROJECT_DIR = os.path.dirname(current_dir)
APPMOCK_DIR = os.path.join(PROJECT_DIR, 'appmock')
BAMBOOS_DIR = os.path.join(PROJECT_DIR, 'bamboos')
DOCKER_DIR = os.path.join(BAMBOOS_DIR, 'docker')
TEST_DIR = os.path.join(PROJECT_DIR, "tests")
UTILS_DIR = os.path.join(TEST_DIR, "utils")
CUCUMBER_DIR = os.path.join(TEST_DIR, "cucumber")
ACCEPTANCE_DIR = os.path.join(TEST_DIR, "acceptance")
PERFORMANCE_DIR = os.path.join(TEST_DIR, "performance")
DEFAULT_CUCUMBER_ENV_DIR = os.path.join(CUCUMBER_DIR, "default_environments")
CUSTOM_CUCUMBER_ENV_DIR = os.path.join(CUCUMBER_DIR, "custom_environments")
CUCUMBER_LOGDIR = os.path.join(CUCUMBER_DIR, "logs")
ACCEPTANCE_LOGDIR = os.path.join(ACCEPTANCE_DIR, "logs")
PERFORMANCE_LOGDIR = os.path.join(PERFORMANCE_DIR, "logs")
PERFORMANCE_ENV_DIR = os.path.join(PERFORMANCE_DIR, "environments")
PERFORMANCE_OUTPUT = os.path.join(PERFORMANCE_LOGDIR, "performance.json")
EXAMPLE_ENV_DIR = os.path.join(BAMBOOS_DIR, "example_env")

# env_up log files
PREPARE_ENV_LOG_FILE = "prepare_test_environment.log"
PREPARE_ENV_ERROR_LOG_FILE = "prepare_test_environment_error.log"

# Append useful modules to the path
sys.path = [PROJECT_DIR, DOCKER_DIR] + sys.path
