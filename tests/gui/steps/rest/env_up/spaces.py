"""Steps for spaces creation using REST API.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import time
import yaml

import requests

from tests.gui.utils.onezone_client import (SpaceCreateRequest,
                                            SpacePrivileges,
                                            DefaultSpace)
from tests.gui.utils.onepanel_client import SpaceSupportRequest

from pytest_bdd import given, parsers

from ..common import get_oz_user_api, get_oz_space_api, get_op_panel_api
from ..exceptions import checked_call, HTTPNotFound


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
    admin_panel_api = get_oz_space_api(admin_credentials.username,
                                       admin_credentials.password,
                                       zone_hostname)

    for space_name, description in yaml.load(config).items():
        owner = users_db[description['owner']]
        user_zone_api = get_oz_space_api(owner.username, owner.password,
                                         zone_hostname)

        spaces_db[space_name] = space_id = _create_space(owner, space_name,
                                                         zone_hostname)
        _add_users_to_space(admin_panel_api, space_id, users_db,
                            description.get('users', {}))
        _set_as_home_for_users(space_id, zone_hostname, users_db,
                               description.get('home space for', []))
        _add_groups_to_space(admin_panel_api, space_id, groups_db,
                             description.get('groups', {}))
        _get_support(admin_credentials, user_zone_api, space_id, storages_db,
                     hosts, description.get('providers', {}))
        _init_storage(owner, space_name, hosts['oneprovider'],
                      description.get('storage', {}))


def _create_space(owner_credentials, space_name, zone_hostname):
    user_zone_api = get_oz_user_api(owner_credentials.username,
                                    owner_credentials.password,
                                    zone_hostname)
    user_zone_api.create_user_space(SpaceCreateRequest(name=space_name))

    for space_id in user_zone_api.list_user_spaces().spaces:
        space = user_zone_api.get_user_space(space_id)
        if space.name == space_name:
            return space.space_id


def _add_users_to_space(admin_panel_api, space_id, users_db, users_to_add):
    for user in users_to_add:
        try:
            [(user, options)] = user.items()
        except AttributeError:
            admin_panel_api.add_user_to_space(space_id, users_db[user].id)
        else:
            privileges = SpacePrivileges(privileges=options['privileges'])
            admin_panel_api.add_user_to_space(space_id, users_db[user].id,
                                              privileges=privileges)


def _set_as_home_for_users(space_id, zone_hostname, users_db, users):
    for user in (users_db[username] for username in users):
        user_zone_api = get_oz_user_api(user.username, user.password,
                                        zone_hostname)
        user_zone_api.set_default_space(DefaultSpace(space_id))


def _add_groups_to_space(admin_panel_api, space_id, groups_db, groups_to_add):
    for group in groups_to_add:
        try:
            [(group, options)] = group.items()
        except AttributeError:
            admin_panel_api.add_group_to_space(space_id, groups_db[group])
        else:
            privileges = SpacePrivileges(privileges=options['privileges'])
            admin_panel_api.add_group_to_space(space_id, groups_db[group],
                                               group_id=privileges)


def _get_support(admin_credentials, user_client, space_id,
                 storages_db, hosts, providers):
    for provider in providers:
        [(provider, options)] = provider.items()

        provider_hostname = hosts['provider_panel'][provider]
        admin_panel_client = get_op_panel_api(admin_credentials.username,
                                              admin_credentials.password,
                                              provider_hostname)

        storage_name = options['storage']
        try:
            storage_id = storages_db[storage_name]
        except KeyError:
            storage_id = storages_db[storage_name] = \
                _get_storage_id(storage_name, admin_panel_client)

        token = user_client.create_space_support_token(space_id)
        support_request = SpaceSupportRequest(token=token.token,
                                              size=int(options['size']),
                                              storage_id=storage_id)
        admin_panel_client.support_space(support_request)


def _get_storage_id(storage_name, admin_panel_client):
    for storage_id in admin_panel_client.get_storages().ids:
        storage = admin_panel_client.get_storage_details(storage_id)
        if storage.name == storage_name:
            return storage.id


def _init_storage(owner_credentials, space_name, hosts, storage_conf):
    if not storage_conf:
        return

    defaults = storage_conf['defaults']
    provider_hostname = hosts[defaults['provider']]

    def create_cdmi_object(path, data=None, repeats=10,
                           auth=(owner_credentials.username,
                                 owner_credentials.password),
                           url='https://{}:8443/cdmi/'
                               ''.format(provider_hostname)):
        result = None
        for _ in xrange(repeats):
            try:
                result = checked_call(requests.put, url + path, auth=auth,
                                      data=data, verify=False)
            except HTTPNotFound:
                # because user may not yet exist in provider first call
                # will fail, as such wait some time and try again
                time.sleep(1)
            else:
                break

        return result

    _mkdirs(create_cdmi_object, space_name, hosts,
            storage_conf['directory tree'])


def _mkdirs(http_put, cwd, hosts, dir_content=None):
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
            http_put(path + '/')
            _mkdirs(http_put, path, hosts, content)
        else:
            _mkfile(http_put, path, hosts, content)


def _mkfile(http_put, file_path, hosts, file_content=None):
    if file_content:
        try:
            provider = file_content['provider']
        except TypeError:
            http_put(file_path, str(file_content))
        else:
            http_put(file_path, data=file_content.get('content', None),
                     url='https://{}:8443/cdmi/'.format(hosts[provider]))
    else:
        http_put(file_path)
