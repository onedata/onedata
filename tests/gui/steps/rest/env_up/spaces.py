"""Steps for spaces creation using REST API.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import yaml
from functools import partial

import requests

from tests.gui.utils.onezone_client import (SpaceCreateRequest,
                                            SpacePrivileges,
                                            DefaultSpace)
from tests.gui.utils.onepanel_client import SpaceSupportRequest

from pytest_bdd import given, parsers

from ..common import get_oz_user_api, get_oz_space_api, get_op_panel_api
from ..exceptions import checked_call


@given(parsers.parse('initial spaces configuration in "{host}" '
                     'Onezone service:\n{config}'))
def spaces_creation(config, host, admin_credentials, hosts,
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
    _spaces_creation(config, host, admin_credentials, hosts,
                     users, groups, storages, spaces)


def _spaces_creation(config, host, admin_credentials, hosts,
                     users, groups, storages, spaces):

    host = hosts['onezone'][host]
    admin_client = get_oz_space_api(admin_credentials.username,
                                    admin_credentials.password, host)

    for space_name, description in yaml.load(config).items():
        owner = users[description['owner']]
        user_client = get_oz_space_api(owner.username, owner.password, host)

        space_id = _create_space(owner, space_name, host)
        spaces[space_name] = space_id

        _add_users_to_space(admin_client, space_id, users,
                            description.get('users', {}))
        _set_as_home_for_users(space_id, users, host,
                               description.get('home space for', []))
        _add_groups_to_space(admin_client, space_id, groups,
                             description.get('groups', {}))
        _get_support(admin_credentials, user_client, space_id, storages,
                     hosts, description.get('providers', {}))
        _init_storage(owner, space_name, hosts['oneprovider'],
                      description.get('storage', {}))


def _create_space(owner, space_name, oz_host):
    oz_client = get_oz_user_api(owner.username, owner.password, oz_host)
    oz_client.create_user_space(SpaceCreateRequest(name=space_name))

    for space_id in oz_client.list_user_spaces().spaces:
        space = oz_client.get_user_space(space_id)
        if space.name == space_name:
            return space.space_id


def _add_users_to_space(admin_client, space_id, users, users_conf):
    for user in users_conf:
        try:
            [(user, options)] = user.items()
        except AttributeError:
            admin_client.add_user_to_space(space_id, users[user].id)
        else:
            privileges = SpacePrivileges(privileges=options['privileges'])
            admin_client.add_user_to_space(space_id, users[user].id,
                                           privileges=privileges)


def _set_as_home_for_users(space_id, users, oz_host, users_conf):
    for user in (users[user] for user in users_conf):
        oz_client = get_oz_user_api(user.username, user.password, oz_host)
        default_space = DefaultSpace(space_id)
        oz_client.set_default_space(default_space)


def _add_groups_to_space(admin_client, space_id, groups, groups_conf):
    for group in groups_conf:
        try:
            [(group, options)] = group.items()
        except AttributeError:
            admin_client.add_group_to_space(space_id, groups[group])
        else:
            privileges = SpacePrivileges(privileges=options['privileges'])
            admin_client.add_group_to_space(space_id, groups[group],
                                            group_id=privileges)


def _get_support(admin_cred, user_client, space_id,
                 storages, hosts, providers_conf):
    for provider in providers_conf:
        [(provider, options)] = provider.items()
        op_panel_client = get_op_panel_api(admin_cred.username,
                                           admin_cred.password,
                                           hosts['provider_panel'][provider])

        storage_name = options['storage']
        try:
            storage_id = storages[storage_name]
        except KeyError:
            storage_id = storages[storage_name] = \
                _get_storage_id_with_given_name(storage_name,
                                                op_panel_client)

        token = user_client.create_space_support_token(space_id)
        support_request = SpaceSupportRequest(token=token.token,
                                              size=int(options['size']),
                                              storage_id=storage_id)
        op_panel_client.support_space(support_request)


def _get_storage_id_with_given_name(storage_name, rest_client):
    for storage_id in rest_client.get_storages().ids:
        storage = rest_client.get_storage_details(storage_id)
        if storage.name == storage_name:
            return storage.id


def _init_storage(owner, space_name, hosts, storage_conf):
    if not storage_conf:
        return

    auth = (owner.username, owner.password)
    defaults = storage_conf['defaults']
    provider_ip = hosts[defaults['provider']]
    default_url = 'https://{}:8443/cdmi/'.format(provider_ip)
    register_fun = partial(register_user_in_provider, user=owner)
    register_fun(provider_ip=provider_ip)
    _mkdirs(lambda path, data=None,
                   url=default_url: checked_call(requests.put, url + path,
                                                 auth=auth, data=data,
                                                 verify=False),
            space_name, register_fun, hosts, storage_conf['directory tree'])


def _mkdirs(http_put, cwd, register_user_fun, hosts, dir_content=None):
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
            _mkdirs(http_put, path, register_user_fun, hosts, content)
        else:
            _mkfile(http_put, path, register_user_fun, hosts, content)


def _mkfile(http_put, file_path, register_user_fun, hosts,
            file_content=None):
    if file_content:
        try:
            provider = file_content['provider']
        except TypeError:
            http_put(file_path, str(file_content))
        else:
            content = file_content.get('content', None)
            provider_ip = hosts[provider]
            url = 'https://{}:8443/cdmi/'.format(provider_ip)
            register_user_fun(provider_ip=provider_ip)
            http_put(file_path, content, url)
    else:
        http_put(file_path)


def register_user_in_provider(user, provider_ip):
    """Make call to provider service in order to create user in it."""
    auth = (user.username, user.password)
    url = 'https://{}:8443/api/v3/oneprovider/spaces'.format(provider_ip)
    checked_call(requests.get, url, auth=auth, verify=False)
