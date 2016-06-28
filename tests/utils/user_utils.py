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

import json


class User:
    def __init__(self, name, client_node=None, client=None, id=None,
                 oz_domain=None, password=None, provider=None, cert_file=None,
                 key_file=None):

        self.name = name
        if client_node:
            self.clients = {client_node: client}
        else:
            self.clients = {}
        self.last_operation_failed = False
        self.last_op_ret_code = 0
        self.spaces = {}
        self.created_spaces = {}
        self.id = id
        self.tokens = {'support': {},
                       'creation': {},
                       'space_invite': {}}
        self.provider = provider
        self.oz_domain = oz_domain
        self.password = password
        self.headers = dict(DEFAULT_HEADERS)
        self.cert_file = cert_file
        self.key_file = key_file

    def get_client(self, client_node):
        return self.clients.get(client_node, None)

    def get_provider_id(self):
        return self.provider.id

    def get_op_domain(self):
        return self.provider.domain

    def set_oz_domain(self, environment):
        # current version is for one OZ
        self.oz_domain = environment['oz_worker_nodes'][0].split(".", 1)[-1]

    def update_clients(self, client_instance, client):
        self.clients.update({client_instance: client})

    def get_certs_from_provider(self):
        self.cert_file = self.provider.cert_file
        self.key_file = self.provider.key_file

    def mark_last_operation_failed(self):
        self.last_operation_failed = True

    def mark_last_operation_succeeded(self):
        self.last_operation_failed = False


def create_user(user_name, password, onepanel):

    data = {
        "username": user_name,
        "password": password,
        "userRole": "regular"
    }
    status_code, _, body = http_post(onepanel, PANEL_REST_PORT,
                                     panel_rest_path("user"),
                                     data=json.dumps(data),
                                     headers=DEFAULT_HEADERS,
                                     auth=("admin", "password"))
    assert status_code == 204
    return User(user_name, password=password)


def delete_user(user_name, onepanel):

    status_code, _, _ = http_delete(onepanel, PANEL_REST_PORT,
                                    panel_rest_path("user", user_name),
                                    headers=DEFAULT_HEADERS,
                                    auth=("admin", "password"))
    assert status_code == 204


def get_id(user):
    status_code, response_headers, body =\
        http_get(user.oz_domain, OZ_REST_PORT,
                 oz_rest_path("user"),
                 headers=user.headers,
                 cert=(user.cert_file, user.key_file),
                 auth=(user.name, user.password))
    assert status_code == 200

    return json.loads(body)['userId']
