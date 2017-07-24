"""Steps for spaces creation using REST API.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import time
import yaml
import json

from pytest_bdd import given, parsers

from tests import OZ_REST_PORT, PANEL_REST_PORT
from ..utils import (http_get, http_post, http_put,
                     get_panel_rest_path, get_zone_rest_path)
from ..exceptions import HTTPNotFound, HTTPError, HTTPBadRequest


@given(parsers.parse('initial spaces configuration in "{zone_host}" '
                     'Onezone service:\n{config}'))
def create_and_configure_spaces(config, zone_host, admin_credentials, hosts,
                                users, groups, storages, spaces):
    """Create and configure spaces according to given config.

    Config format given in yaml is as follow:

        space_name_1:
            owner: user_name                ---> currently we identify user account with concrete
                                                 browser so user_name == browser_id
            [users]:                        ---> optional
                - user_name_2
                - user_name_3:
                    [privileges]:           ---> optional
                        - privilege_1
                        - privilege_2
            [home space for]:               ---> optional
                - user_name_1
                - user_name_2
            [groups]:                       ---> optional
                - group_name_1:
                    [privileges]:           ---> optional
                        - privilege_1
                        - privilege_2
                - group_name_2
            [providers]:                    ---> optional
                - provider_name_1:
                    storage: name
                    size: size in bits
            [storage]:                      ---> optional
                defaults:
                    provider: provider_name         --> default provider on whose storage
                                                        files will be created
                directory tree:
                    - dir0                  ---> name starting with 'dir' prefix
                                                 is treated as directory
                    - dir2:
                        - file0: text
                        - file1:
                            provider: p2    ---> mandatory if dict form is used
                            content: text
                        - file2

        space_name_2:
            ...

    Example configuration:

        space1:
            owner: user1
            users:
                - user2
                - user3
            home space for:
                - user1
            groups:
                - group1
            providers:
                - p1:
                    storage: onestorage
                    size: 1000000000
            storage:
                defaults:
                    provider: p1
                directory tree:
                    - dir1
                    - dir2:
                        - file0
                        - file1: 11111
                        - file2:
                            provider: 2
                            content: 22222
    """
    _create_and_configure_spaces(config, zone_host, admin_credentials, hosts,
                                 users, groups, storages, spaces)


def _create_and_configure_spaces(config, zone_name, admin_credentials, hosts,
                                 users_db, groups_db, storages_db, spaces_db):

    zone_hostname = hosts['onezone'][zone_name]

    for space_name, description in yaml.load(config).items():
        owner = users_db[description['owner']]
        spaces_db[space_name] = space_id = _create_space(zone_hostname,
                                                         owner.username,
                                                         owner.password,
                                                         space_name)
        _add_users_to_space(zone_hostname, admin_credentials, space_id,
                            users_db, description.get('users', {}))
        _set_as_home_for_users(space_id, zone_hostname, users_db,
                               description.get('home space for', []))
        _add_groups_to_space(zone_hostname, admin_credentials, space_id,
                             groups_db, description.get('groups', {}))
        _get_support(zone_hostname, admin_credentials, owner, space_id,
                     storages_db, hosts, description.get('providers', {}))
        _init_storage(owner, space_name, hosts['oneprovider'],
                      description.get('storage', {}))


def _create_space(zone_hostname, owner_username, owner_password, space_name):
    space_properties = {'name': space_name}
    response = http_post(ip=zone_hostname, port=OZ_REST_PORT,
                         path=get_zone_rest_path('spaces'),
                         auth=(owner_username, owner_password),
                         data=json.dumps(space_properties))
    return response.headers['location'].split('/')[-1]


def _add_users_to_space(zone_hostname, admin_credentials, space_id,
                        users_db, users_to_add):
    for user in users_to_add:
        try:
            [(user, options)] = user.items()
        except AttributeError:
            privileges = None
        else:
            privileges = options['privileges']

        _add_user_to_space(zone_hostname, admin_credentials.username,
                           admin_credentials.password, space_id,
                           users_db[user].id, privileges)


def _add_user_to_space(zone_hostname, admin_username, admin_password,
                       space_id, user_id, privileges):
    if privileges:
        data = json.dumps({'operation': 'set',
                           'privileges': privileges})
    else:
        data = None

    http_put(ip=zone_hostname, port=OZ_REST_PORT,
             path=get_zone_rest_path('spaces', space_id, 'users', user_id),
             auth=(admin_username, admin_password), data=data)


def _set_as_home_for_users(space_id, zone_hostname, users_db, users):
    for user in (users_db[username] for username in users):
        http_put(ip=zone_hostname, port=OZ_REST_PORT,
                 path=get_zone_rest_path('user', 'default_space'),
                 auth=(user.username, user.password),
                 data=json.dumps({'spaceId': space_id}))


def _add_groups_to_space(zone_hostname, admin_credentials, space_id,
                         groups_db, groups_to_add):
    for group in groups_to_add:
        try:
            [(group, options)] = group.items()
        except AttributeError:
            privileges = None
        else:
            privileges = options['privileges']

        _add_group_to_space(zone_hostname, admin_credentials.username,
                            admin_credentials.password, space_id,
                            groups_db[group], privileges)


def _add_group_to_space(zone_hostname, admin_username, admin_password,
                        space_id, group_id, privileges):
    if privileges:
        data = json.dumps({'operation': 'set',
                           'privileges': privileges})
    else:
        data = None

    http_put(ip=zone_hostname, port=OZ_REST_PORT,
             path=get_zone_rest_path('spaces', space_id, 'groups', group_id),
             auth=(admin_username, admin_password), data=data)


def _get_support(zone_hostname, admin_credentials, owner_credentials,
                 space_id, storages_db, hosts, providers):
    admin_username = admin_credentials.username
    admin_password = admin_credentials.password

    for provider in providers:
        [(provider, options)] = provider.items()

        provider_hostname = hosts['oneprovider'][provider]
        storage_name = options['storage']

        try:
            storage_id = storages_db[storage_name]
        except KeyError:
            storage_id = storages_db[storage_name] = \
                _get_storage_id(provider_hostname, admin_username,
                                admin_password, storage_name)

        token = http_post(ip=zone_hostname, port=OZ_REST_PORT,
                          path=get_zone_rest_path('spaces', space_id,
                                                  'providers', 'token'),
                          auth=(owner_credentials.username,
                                owner_credentials.password)).json()['token']

        space_support_details = {'token': token,
                                 'size': int(options['size']),
                                 'storageId': storage_id}
        http_post(ip=provider_hostname, port=PANEL_REST_PORT,
                  path=get_panel_rest_path('provider', 'spaces'),
                  auth=(admin_username, admin_password),
                  data=json.dumps(space_support_details))


def _get_storage_id(provider_hostname, admin_username,
                    admin_password, storage_name):
    storages_id = http_get(ip=provider_hostname, port=PANEL_REST_PORT,
                           path=get_panel_rest_path('provider', 'storages'),
                           auth=(admin_username, admin_password))
    for storage_id in storages_id.json()['ids']:
        storage_details = http_get(ip=provider_hostname, port=PANEL_REST_PORT,
                                   path=get_panel_rest_path('provider',
                                                            'storages',
                                                            storage_id),
                                   auth=(admin_username, admin_password))
        if storage_details.json()['name'] == storage_name:
            return storage_id


def _init_storage(owner_credentials, space_name, hosts, storage_conf):
    if not storage_conf:
        return

    # if we make call to fast after deleting users from previous test
    # provider cache was not refreshed and call will create dir for
    # now nonexistent user, to avoid this wait some time
    import time; time.sleep(2)

    defaults = storage_conf['defaults']
    provider_hostname = hosts[defaults['provider']]

    def create_cdmi_object(path, data=None, repeats=10,
                           auth=(owner_credentials.username,
                                 owner_credentials.password)):
        response = None
        for _ in xrange(repeats):
            try:
                response = http_put(ip=provider_hostname, port=8443,
                                    path='/cdmi/'+path, auth=auth,
                                    data=data)
            except (HTTPNotFound, HTTPBadRequest):
                # because user may not yet exist in provider first call
                # will fail, as such wait some time and try again
                time.sleep(1)
            else:
                break

        return response

    _mkdirs(create_cdmi_object, space_name, hosts,
            storage_conf['directory tree'])


def _mkdirs(create_cdmi_obj, cwd, hosts, dir_content=None):
    if not dir_content:
        return

    for item in dir_content:
        try:
            [(name, content)] = item.items()
        except AttributeError:
            name = item
            content = None

        path = cwd + '/' + name
        if name.startswith('dir'):
            create_cdmi_obj(path + '/')
            _mkdirs(create_cdmi_obj, path, hosts, content)
        else:
            _mkfile(create_cdmi_obj, path, hosts, content)


def _mkfile(create_cdmi_obj, file_path, hosts, file_content=None):
    if file_content:
        try:
            provider = file_content['provider']
        except TypeError:
            create_cdmi_obj(file_path, str(file_content))
        else:
            create_cdmi_obj(file_path, data=file_content.get('content', None),
                            url='https://{}:8443/cdmi/'.format(hosts[provider]))
    else:
        create_cdmi_obj(file_path)
