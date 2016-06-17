"""Module implements utility functions for managing spaces in onedata via REST.
"""
from tests.utils.user_utils import get_first_op_erl_node

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
from tests import *
from tests.utils.net_utils import http_post, http_get, http_delete, http_put
from tests.utils.string_utils import parse
from tests.utils.utils import get_storages, get_op_cookie

from environment import docker

import subprocess
import json


def create_space(user, space_name):
    data = {'name': space_name}
    status_code, response_headers, body = \
        http_post(user.oz_domain, OZ_REST_PORT, "/spaces",
                  data=json.dumps(data), headers=user.headers,
                  cert=(user.cert_file, user.key_file))

    assert status_code == 201

    response_path = response_headers['location']
    return parse(r'/spaces/(.*)', response_path, 1)


def request_support(user, space_name):
    if space_name not in user.spaces:
        user.spaces[space_name] = get_default_space(user)
    space_id = user.spaces[space_name]
    status_code, _, body, = \
        http_get(user.oz_domain, OZ_REST_PORT, "/spaces/{}/providers/token"
                 .format(space_id), headers=user.headers,
                 cert=(user.cert_file, user.key_file))

    assert 200 == status_code
    return json.loads(body)['token']


def support_space(user, space_name, provider, size, env_description_file,
                  environment):

    data = {'token': user.tokens['support'][space_name],
            'size': str(size)}

    status_code, response_headers, body = http_post(user.oz_domain,
                                                    OZ_REST_PORT,
                                                    "/provider/spaces/support",
                                                    data=json.dumps(data),
                                                    headers=DEFAULT_HEADERS,
                                                    cert=(user.cert_file,
                                                          user.key_file))

    response_path = response_headers['location']
    space_id = parse(r'/provider/spaces/(.*)', response_path, 1)

    op_node = get_first_op_erl_node(user.op_domain, environment)
    op_cookie = get_op_cookie(env_description_file, user.op_domain, False)

    # create storages mapping
    env_conf_input = {}

    # storage_name = environment['storages']['posix'].keys()[0]
    storage_name = get_storages(env_description_file, provider)[0]['name']
    provider_conf = {
        user.provider_id: {
            'nodes': [op_node],
            'cookie': op_cookie
        }
    }
    env_conf_input['provider_domains'] = provider_conf

    space_conf = {
        space_id: {
            'providers': {
                user.provider_id: {
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

    assert 201 == status_code


def invite_to_space(user, user_to_invite, space_name):
    if space_name not in user.spaces:
        user.spaces[space_name] = get_default_space(user)
    space_id = user.spaces[space_name]
    status_code, headers, body, = http_get(user.oz_domain, OZ_REST_PORT,
                                           "/spaces/{}/users/token"
                                           .format(space_id),
                                           headers=user.headers,
                                           cert=(user.cert_file, user.key_file))
    assert 200 == status_code
    return json.loads(body)['token']


def join_space(user, space_name):
    data = {'token': user.tokens['space_invite'][space_name]}
    status_code, _, body, = http_post(user.oz_domain, OZ_REST_PORT,
                                      "/user/spaces/join",
                                      data=json.dumps(data),
                                      headers=user.headers, cert=(
            user.cert_file, user.key_file))
    assert status_code == 201


def get_default_space(user):
    status_code, headers, body, = http_get(user.oz_domain, OZ_REST_PORT,
                                           "/user/spaces/default",
                                           headers=user.headers,
                                           cert=(user.cert_file, user.key_file))
    assert status_code == 200
    return json.loads(body)['spaceId']


def remove_user(user, user_to_remove, space_name):
    if space_name not in user.spaces:
        user.spaces[space_name] = get_default_space(user)
    space_id = user.spaces[space_name]
    status_code, _, _, = http_delete(user.oz_domain, OZ_REST_PORT,
                                     "/spaces/{space_id}/users/{user_id}"
                                     .format(space_id=space_id,
                                             user_id=user_to_remove.id),
                                     headers=user.headers,
                                     cert=(user.cert_file, user.key_file))
    assert 202 == status_code


def delete_space(user, space_name):
    if space_name not in user.spaces:
        user.spaces[space_name] = get_default_space(user)
    space_id = user.spaces[space_name]
    status_code, headers, body, = http_delete(user.oz_domain, OZ_REST_PORT,
                                              "/spaces/{space_id}"
                                              .format(space_id=space_id),
                                              headers=user.headers,
                                              cert=(user.cert_file,
                                                    user.key_file))
    assert status_code == 202
    del user.spaces[space_name]


def assign_privileges(user1, user2, privileges, space_name):
    space_id = user1.spaces[space_name]
    data = {'privileges': privileges}
    status_code, _, _, = \
        http_put(user1.oz_domain, OZ_REST_PORT,
                 "/spaces/{sid}/users/{uid}/privileges"
                 .format(sid=space_id, uid=user2.id),
                 True, headers=user1.headers,
                 cert=(user1.cert_file, user1.key_file),
                 data=json.dumps(data))

    assert status_code == 204
