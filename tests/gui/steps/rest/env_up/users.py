"""Steps for users creation using REST API.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import yaml
import json
from functools import partial
from collections import namedtuple

from pytest_bdd import given, parsers
from pytest import skip

from tests import OZ_REST_PORT, PANEL_REST_PORT
from ..utils import (http_get, http_post, http_delete, http_patch,
                     get_panel_rest_path, get_zone_rest_path)
from ..exceptions import HTTPError, HTTPNotFound
from tests.gui.utils.generic import repeat_failed


UserCred = namedtuple('UserCredentials', ['username', 'password', 'id'])


@given(parsers.parse('initial users configuration in "{host}" '
                     'Onezone service:\n{config}'))
def users_creation(host, config, admin_credentials, hosts, users):
    zone_hostname = hosts['onezone'][host]

    users_db = {}
    for user_config in yaml.load(config):
        username, options = _parse_user_info(user_config)
        try:
            user_cred = _create_user(zone_hostname, admin_credentials,
                                     username, options)
            _configure_user(zone_hostname, user_cred, options)
        except Exception as ex:
            _rm_users(zone_hostname, admin_credentials, users_db)
            raise ex
        else:
            users[username] = users_db[username] = user_cred

    yield

    _rm_users(zone_hostname, admin_credentials, users_db)


def _parse_user_info(user_config):
    try:
        [(username, options)] = user_config.items()
    except AttributeError:
        return user_config, {}
    else:
        return username, options


def _create_user(zone_hostname, admin_credentials, username, options):
    password = options.get('password', 'password')
    user_conf_details = {'username': username,
                         'password': password,
                         'userRole': options.get('user role', 'regular')}
    # if user already exist (possible remnants of previous tests) skip test
    try:
        http_get(ip=zone_hostname, port=PANEL_REST_PORT,
                 path=get_panel_rest_path('users', username),
                 auth=(admin_credentials.username, admin_credentials.password))
    except HTTPNotFound:
        http_post(ip=zone_hostname, port=PANEL_REST_PORT,
                  path=get_panel_rest_path('users'),
                  auth=(admin_credentials.username, admin_credentials.password),
                  data=json.dumps(user_conf_details))

        # user is created in zone panel and not zone itself
        # so for them to be created also in zone
        # login/rest call to zone using his credentials must be made
        response = http_get(ip=zone_hostname, port=OZ_REST_PORT,
                            path=get_zone_rest_path('user'),
                            auth=(username, password)).json()
        user_id = response['userId']
        return UserCred(username=username, password=password, id=user_id)

    except HTTPError:
        skip('failed to create "{}" user'.format(username))
    else:
        skip('"{}" user already exist'.format(username))


def _configure_user(zone_hostname, user_cred, options):
    alias = options.get('alias', None)
    if alias:
        try:
            http_patch(ip=zone_hostname, port=OZ_REST_PORT,
                       path=get_zone_rest_path('user'),
                       auth=(user_cred.username, user_cred.password),
                       data=json.dumps({'alias': alias}))
        except HTTPError:
            skip('"{}" alias is already occupied'.format(alias))


def _rm_users(zone_hostname, admin_credentials, users_db,
              ignore_http_exceptions=False):
    for user_credentials in users_db.values():
        _rm_user(zone_hostname, admin_credentials, user_credentials,
                 ignore_http_exceptions)


def _rm_user(zone_hostname, admin_credentials, user_credentials,
             ignore_http_exceptions=False):
    admin_username = admin_credentials.username
    admin_password = admin_credentials.password
    rm_zone_user = partial(_rm_zone_user, user_id=user_credentials.id)
    rm_panel_user = partial(_rm_panel_user, username=user_credentials.username)

    for fun in (rm_zone_user, rm_panel_user):
        try:
            fun(zone_hostname, admin_username, admin_password)
        except HTTPError as ex:
            if not ignore_http_exceptions:
                raise ex


@repeat_failed(attempts=5)
def _rm_panel_user(zone_hostname, admin_username, admin_password, username):
    path = get_panel_rest_path('users', username)
    http_delete(ip=zone_hostname, port=PANEL_REST_PORT, path=path,
                auth=(admin_username, admin_password))


@repeat_failed(attempts=5)
def _rm_zone_user(zone_hostname, admin_username, admin_password, user_id):
    path = get_zone_rest_path('users', user_id)
    http_delete(ip=zone_hostname, port=OZ_REST_PORT, path=path,
                auth=(admin_username, admin_password))
