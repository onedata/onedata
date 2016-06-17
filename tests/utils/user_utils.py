"""Module implements utility functions for managing users in onedata via REST.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from tests import *
from tests.utils.net_utils import (http_post, http_delete, oz_rest_path,
                                   http_get, panel_rest_path)

from environment import docker

import base64
import json
import tempfile


class User:
    def __init__(self, name, client_node=None, client=None, headers=None,
                 id=None, provider_id=None, op_domain=None, oz_domain=None,
                 password=None, op_worker=None):

        self.name = name
        if client_node:
            self.clients = {client_node: client}
        self.last_op_ret_code = 0
        self.files = {}
        self.spaces = {}
        self.headers = headers
        self.id = id
        self.tokens = {'support': {},
                       'creation': {},
                       'space_invite': {}}
        self.provider_id = provider_id
        self.op_domain = op_domain
        self.oz_domain = oz_domain
        self.password = password
        self.op_worker = op_worker

    def get_client(self, client_node):
        return self.clients.get(client_node, None)

    def set_op_domain(self, environment, provider_id):
        self.op_domain = environment['domain_mappings'][provider_id]

    def set_op_worker(self, environment, op_domain):
        self.op_worker = get_first_op_worker(op_domain, environment)

    def set_oz_domain(self, environment):
        # current version is for one OZ
        self.oz_domain = environment['oz_worker_nodes'][0].split(".", 1)[-1]


def create_user(user_name, password, onepanel):

    data = {
        "username": user_name,
        "password": password,
        "userRole": "admin"  #todo maybe userrole=user is enough ???
    }
    return_code, _, _ = http_post(onepanel, PANEL_REST_PORT,
                                  panel_rest_path("user"),
                                  data=json.dumps(data),
                                  headers=DEFAULT_HEADERS,
                                  auth=("admin", "password"))
    assert return_code == 204
    print "CREATED: ", user_name, return_code
    return User(user_name, password=password)


def delete_user(user_name, onepanel):

    return_code, _, _ = \
        http_delete(onepanel, PANEL_REST_PORT,
                    panel_rest_path("user", user_name),
                    headers=DEFAULT_HEADERS,
                    auth=("admin", "password"))
    assert return_code == 204


def authorize_user(user):
    """
    :type user: client_utils.User
    """

    basic_auth = "Basic {user}:{password}".format(user=user.name,
                                                  password=user.password)
    basic_auth = base64.b64encode(basic_auth)
    print basic_auth, user.name, user.password
    headers = {"Authorization": basic_auth}.update(DEFAULT_HEADERS)

    return_code, _, body = http_get(user.oz_domain, OZ_REST_PORT,
                                    oz_rest_path("user", "client_token"),
                                    headers=headers,
                                    auth=("admin", "password"))

    assert return_code == 200
    return json.loads(body)['token']


def get_provider_certs(user, provider_id):

    tempdir = tempfile.mkdtemp()
    docker.cp(user.op_worker, PROVIDER_KEY_PATH, tempdir, False)
    docker.cp(user.op_worker, PROVIDER_CERT_PATH, tempdir, False)

    cert_file = os.path.join(tempdir, PROVIDER_CERT_FILE)
    key_file = os.path.join(tempdir, PROVIDER_KEY_FILE)

    user.cert_file = cert_file
    user.key_file = key_file

    return tempdir # todo step fixture should delete tempdir


def get_id(user):
    _, _, body = http_get(user.oz_domain, OZ_REST_PORT, "/user", True,
                          headers=user.headers,
                          cert=(user.cert_file, user.key_file))
    return json.loads(body)['userId']


def get_first_op_erl_node(domain, env):
    return get_first_erl_node(domain, env, 'op')


def get_first_oz_erl_node(domain, env):
    return get_first_erl_node(domain, env, 'oz')


def get_first_erl_node(domain, env, key):
    key = {
        'op': 'op_worker_nodes',
        'oz': 'oz_worker_nodes'
    }[key]
    # returns first node from list
    return [node for node in env[key] if node.endswith(domain)][0]


def get_first_op_worker(domain, env):
    return get_first_op_erl_node(domain, env).split('@')[-1]


def get_first_oz_worker(domain, env):
    return get_first_oz_erl_node(domain, env).split('@')[-1]