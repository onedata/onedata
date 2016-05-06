# import os
# import sys
# from tests import PROJECT_DIR, BAMBOOS_DIR, APPMOCK_DIR, DOCKER_DIR, TEST_DIR
#
# # env_up log files
# PREPARE_ENV_LOG_FILE = "prepare_test_environment.log"
# PREPARE_ENV_ERROR_LOG_FILE = "prepare_test_environment_error.log"
#
# _script_dir = os.path.dirname(os.path.realpath(__file__))
#
#
#
# # Define variables for use in tests
# # project_dir = os.path.dirname(_script_dir)
# # project_dir = "/home/kuba/IdeaProjects/work/VFS-1918/onedata"
# # appmock_dir = os.path.join(project_dir, 'appmock')
# # bamboos_dir = os.path.join(project_dir, 'bamboos')
# # docker_dir = os.path.join(bamboos_dir, 'docker')
# # test_dir = os.path.join(project_dir, "tests")
# cucumber_dir = os.path.join(TEST_DIR, "cucumber")
# acceptance_dir = os.path.join(TEST_DIR, "acceptance")
# performance_dir = os.path.join(TEST_DIR, "performance")
# default_cucumber_env_dir = os.path.join(cucumber_dir, "default_environments")
# custom_cucumber_env_dir = os.path.join(cucumber_dir, "custom_environments")
# cucumber_logdir = os.path.join(cucumber_dir, "logs")
# acceptance_logdir = os.path.join(acceptance_dir, "logs")
# performance_logdir = os.path.join(performance_dir, "logs")
# performance_env_dir = os.path.join(performance_dir, "environments")
# performance_output = os.path.join(performance_logdir, "performance.json")
# example_env_dir = os.path.join(BAMBOOS_DIR, "example_env")
#
# print "UTILS: "
# print sys.path,'\n'
# # Append useful modules to the path
# sys.path = [PROJECT_DIR, DOCKER_DIR] + sys.path
# print sys.path,'\n'
