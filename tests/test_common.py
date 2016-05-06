import os
import sys

# env_up log files
PREPARE_ENV_LOG_FILE = "prepare_test_environment.log"
PREPARE_ENV_ERROR_LOG_FILE = "prepare_test_environment_error.log"

_script_dir = os.path.dirname(os.path.realpath(__file__))

# Define variables for use in tests
project_dir = os.path.dirname(_script_dir)
appmock_dir = os.path.join(project_dir, 'appmock')
bamboos_dir = os.path.join(project_dir, 'bamboos')
docker_dir = os.path.join(bamboos_dir, 'docker')
test_dir = os.path.join(project_dir, "tests")
cucumber_dir = os.path.join(test_dir, "cucumber")
acceptance_dir = os.path.join(test_dir, "acceptance")
performance_dir = os.path.join(test_dir, "performance")
default_cucumber_env_dir = os.path.join(cucumber_dir, "default_environments")
custom_cucumber_env_dir = os.path.join(cucumber_dir, "custom_environments")
cucumber_logdir = os.path.join(cucumber_dir, "logs")
acceptance_logdir = os.path.join(acceptance_dir, "logs")
performance_logdir = os.path.join(performance_dir, "logs")
performance_env_dir = os.path.join(performance_dir, "environments")
performance_output = os.path.join(performance_logdir, "performance.json")
example_env_dir = os.path.join(bamboos_dir, "example_env")

# Append useful modules to the path
sys.path = [project_dir, docker_dir] + sys.path
