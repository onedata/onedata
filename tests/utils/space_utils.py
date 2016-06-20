"""Module implements utility functions for managing spaces in onedata via REST.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from tests import *
from tests.utils.net_utils import http_post, http_get, http_delete, http_put, \
    oz_rest_path
from tests.utils.string_utils import parse
from tests.utils.utils import get_storages, get_op_cookie, get_first_op_erl_node

from environment import docker

import subprocess
import json


def create_space(user, space_name):
    data = {'name': space_name}
    status_code, response_headers, body = \
        http_post(user.oz_domain, OZ_REST_PORT, oz_rest_path("spaces"),
                  data=json.dumps(data),
                  headers=user.headers,
                  cert=(user.cert_file, user.key_file),
                  auth=(user.name, user.password))

    assert status_code == 201

    response_path = response_headers['location']
    return parse(r'/spaces/(.*)', response_path, 1)


def request_support(user, space_name):
    space_id = user.spaces[space_name]
    status_code, _, body, = http_get(user.oz_domain,
                                     OZ_REST_PORT,
                                     oz_rest_path("spaces", space_id,
                                                  "providers", "token"),
                                     headers=user.headers,
                                     cert=(user.cert_file, user.key_file),
                                     auth=(user.name, user.password))

    assert 200 == status_code
    return json.loads(body)['token']


def support_space(user, space_name, provider, size, env_description_file,
                  environment):
    data = {'token': user.tokens['support'][space_name],
            'size': str(size)}

    status_code, response_headers, body =\
        http_post(user.oz_domain,
                  OZ_REST_PORT,
                  oz_rest_path("provider", "spaces", "support"),
                  data=json.dumps(data),
                  headers=DEFAULT_HEADERS,
                  cert=(provider.cert_file, provider.key_file))

    assert 201 == status_code

    response_path = response_headers['location']
    space_id = parse(r'/provider/spaces/(.*)', response_path, 1)

    op_node = get_first_op_erl_node(user.get_op_domain(), environment)
    op_cookie = get_op_cookie(env_description_file, user.get_op_domain(), False)

    # create storages mapping
    env_conf_input = {}

    storage_name = get_storages(env_description_file, provider.id)[0]['name']
    provider_conf = {
        user.get_provider_id(): {
            'nodes': [op_node],
            'cookie': op_cookie
        }
    }
    env_conf_input['provider_domains'] = provider_conf

    space_conf = {
        space_id: {
            'providers': {
                user.get_provider_id(): {
                    'storage': storage_name,
                    'supported_size': size
                }
            }
        }
    }
    env_conf_input['spaces'] = space_conf

    subprocess.check_output(['escript', ENV_CONFIGURATOR_ESCRIPT,
                             json.dumps(env_conf_input), str(False),
                             str(False)])

    return space_id


def invite_to_space(user, user_to_invite, space_name):
    space_id = user.spaces[space_name]
    status_code, headers, body = http_get(user.oz_domain, OZ_REST_PORT,
                                          oz_rest_path("spaces", space_id,
                                                       "users", "token"),
                                          headers=user.headers,
                                          cert=(user.cert_file, user.key_file),
                                          auth=(user.name, user.password))
    assert 200 == status_code
    return json.loads(body)['token']


def join_space(user, space_name):
    data = {'token': user.tokens['space_invite'][space_name]}
    status_code, response_headers, _ =\
        http_post(user.oz_domain, OZ_REST_PORT,
                  oz_rest_path("user", "spaces", "join"),
                  data=json.dumps(data),
                  headers=user.headers,
                  cert=(user.cert_file,user.key_file),
                  auth=(user.name, user.password))

    response_path = response_headers['location']
    user.spaces[space_name] = parse(r'/user/spaces/(.*)', response_path, 1)
    assert status_code == 201


def get_default_space(user):
    status_code, headers, body, = http_get(user.oz_domain, OZ_REST_PORT,
                                           oz_rest_path("user", "spaces",
                                                        "default"),
                                           headers=user.headers,
                                           cert=(user.cert_file, user.key_file),
                                           auth=(user.name, user.password))
    assert status_code == 200
    return json.loads(body)['spaceId']


def remove_user(user, user_to_remove, space_name):
    space_id = user.spaces[space_name]
    status_code, _, _, = http_delete(user.oz_domain, OZ_REST_PORT,
                                     oz_rest_path("spaces", space_id, "users",
                                                  user_to_remove.id),
                                     headers=user.headers,
                                     cert=(user.cert_file, user.key_file),
                                     auth=(user.name, user.password))
    assert 202 == status_code


def delete_space(user, space_name):
    space_id = user.spaces[space_name]
    status_code, headers, body, = http_delete(user.oz_domain, OZ_REST_PORT,
                                              oz_rest_path("spaces", space_id),
                                              headers=user.headers,
                                              cert=(user.cert_file,
                                                    user.key_file),
                                              auth=(user.name, user.password))
    assert status_code == 202


def assign_privileges(user1, user2, privileges, space_name):
    space_id = user1.spaces[space_name]
    data = {'privileges': privileges}
    status_code, _, _, = \
        http_put(user1.oz_domain, OZ_REST_PORT,
                 oz_rest_path("spaces", space_id, "users", user2.id,
                              "privileges"),
                 data=json.dumps(data),
                 headers=user1.headers,
                 cert=(user1.cert_file, user1.key_file),
                 auth=(user1.name, user1.password))

    assert status_code == 204


def unsupport_space(provider, space):
    space_id = provider.spaces[space]

    status_code, headers, body = \
        http_delete(provider.oz_domain, OZ_REST_PORT,
                    oz_rest_path("provider", "spaces", space_id),
                    cert=(provider.cert_file, provider.key_file),
                    headers=DEFAULT_HEADERS)
    assert status_code == 202
