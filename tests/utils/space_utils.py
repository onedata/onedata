"""Module implements utility functions for managing spaces in onedata via REST.
"""
from tests.utils.net_utils import http_post, http_get, http_delete
from tests.utils.string_utils import parse

import json

__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *


def create_space(user, space_name):
    data = {'name': space_name}
    status_code, response_headers, body = http_post(user.oz_domain,
                                                    REST_PORT, "/spaces", True,
                                                    data=json.dumps(data),
                                                    headers=user.headers,
                                                    cert=(user.cert_file,
                                                          user.key_file))
    print status_code
    print response_headers
    print body

    assert status_code == 201

    response_path = response_headers['location']
    return parse(r'/spaces/(.*)', response_path, 1)


def request_support(user, space_name):
    if space_name not in user.spaces:
        user.spaces[space_name] = get_default_space(user)
    print user.spaces, dir(user.spaces)
    space_id = user.spaces[space_name]
    status_code, _, body, = http_get(user.oz_domain, REST_PORT,
                                     "/spaces/{}/providers/token".format(
                                             space_id),
                                     True, headers=user.headers,
                                     cert=(user.cert_file, user.key_file))
    assert 200 == status_code
    print json.loads(body)['token']
    return json.loads(body)['token']


def support_space(user, space_name, size):
    data = {'token': user.tokens['support'][space_name],
            'size': str(size)}
    status_code, _, _ = http_post(user.oz_domain, REST_PORT,
                                  "/provider/spaces/support", True,
                                  headers=DEFAULT_HEADERS,
                                  data=json.dumps(data),
                                  cert=(user.cert_file, user.key_file))
    assert 201 == status_code


def invite_to_space(user, user_to_invite, space_name):
    if space_name not in user.spaces:
        user.spaces[space_name] = get_default_space(user)
    space_id = user.spaces[space_name]
    status_code, headers, body, = http_get(user.oz_domain, REST_PORT,
                                           "/spaces/{}/users/token".format(
                                                   space_id),
                                           True, headers=user.headers,
                                           cert=(
                                               user.cert_file, user.key_file))
    assert 200 == status_code
    print json.loads(body)['token']
    return json.loads(body)['token']


def join_space(user, space_name):
    data = {'token': user.tokens['space_invite'][space_name]}
    status_code, _, body, = http_post(user.oz_domain, REST_PORT,
                                      "/user/spaces/join", True,
                                      headers=user.headers,
                                      data=json.dumps(data),
                                      cert=(
                                          user.cert_file, user.key_file))
    assert status_code == 201


def get_default_space(user):
    status_code, headers, body, = http_get(user.oz_domain, REST_PORT,
                                           "/user/spaces/default", True,
                                           headers=user.headers,
                                           cert=(user.cert_file, user.key_file))
    assert status_code == 200
    return json.loads(body)['spaceId']


def remove_user(user, user_to_remove, space_name):
    if space_name not in user.spaces:
        user.spaces[space_name] = get_default_space(user)
    space_id = user.spaces[space_name]
    status_code, _, _, = http_delete(user.oz_domain, REST_PORT,
                                     "/spaces/{space_id}/users/{user_id}"
                                     .format(space_id=space_id,
                                             user_id=user_to_remove.id),
                                     True, headers=user.headers,
                                     cert=(user.cert_file, user.key_file))
    assert 202 == status_code


def delete_space(user, space_name):
    if space_name not in user.spaces:
        user.spaces[space_name] = get_default_space(user)
    space_id = user.spaces[space_name]
    status_code, headers, body, = http_delete(user.oz_domain, REST_PORT,
                                              "/spaces/{space_id}"
                                              .format(space_id=space_id),
                                              True, headers=user.headers,
                                              cert=(user.cert_file,
                                                    user.key_file))
    print "DELETE SPACE: ", status_code, headers, body
