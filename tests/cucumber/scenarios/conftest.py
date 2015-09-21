# import pytest
# # these commands set up paths to scripts from 'bamboos'
# import os
# import sys
# import re
# curr_dir = os.path.dirname(os.path.realpath(__file__))
#
# curr_dir_list = curr_dir.split('/')
# ind = curr_dir_list.index('onedata')
# root_dir = '/'.join(curr_dir_list[:ind+1])
#
# docker_dir = os.path.join(root_dir, 'bamboos', 'docker')
# sys.path.insert(0, docker_dir)
#
# from environment import docker, env
# from steps.common import client_id
#
#
# def pytest_bdd_after_scenario(request, feature, scenario):
#
#     # print feature.name
#     # print scenario.name
#     # print dir(feature)
#     # print dir(scenario)
#
#     if feature.name == 'Directory_CRUD' and scenario.name == 'Create directory':
#         print "CONF:"
#         # print scenario.params
#         # print scenario.steps
#         for step in scenario.steps:
#             if re.match('.*creates directories.*', step.name):
#                 # print step.name
#                 print step.params
#                 # print dir(step)
#
#         # print dir(feature)
#         # print dir(scenario)
#         # print scenario.steps.__getattribute__
#         # print scenario.steps.__contains__
#         #
#         # print dir(scenario.steps)
#
#         # docker.exec_(container=client_id, command="rm -rf ~/onedata/*")
#
# def test(client_id):
#     return client_id