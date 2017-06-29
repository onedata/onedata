"""Steps for groups creation using REST API.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import yaml

from tests.gui.utils.onezone_client import Group, GroupPrivileges
from pytest_bdd import given, parsers

from tests.gui.utils.onezone_client.rest import ApiException
from ..common import get_oz_user_api, get_oz_group_api


@given(parsers.parse('initial groups configuration in "{service}" '
                     'Onezone service:\n{config}'))
def groups_creation(config, service, admin_credentials,
                    users, hosts, groups):
    """Create and configure groups according to given config.

    Config format given in yaml is as follow:

        group_name_1:
            owner: user_name
            [users]:                        ---> optional
                - user_name_2
                - user_name_3:
                    [privileges]:           ---> optional
                        - privilege_1
                        - privilege_2
        group_name_2:
            ...

    Example configuration:

        group1:
            owner: user1
            users:
                - user2:
                    privileges:
                        - group_invite_user
                        - group_remove_user
                - user3
    """
    _groups_creation(config, service, admin_credentials,
                     users, hosts, groups)


def _groups_creation(config, service, admin_credentials,
                     users, hosts, groups):
    zone_host = hosts['onezone'][service]
    admin_client = get_oz_group_api(admin_credentials.username,
                                    admin_credentials.password,
                                    zone_host)

    for group_name, description in yaml.load(config).items():
        group_id = _create_group(users[description['owner']],
                                 group_name, zone_host)
        groups[group_name] = group_id

        for user in description.get('users', {}):
            try:
                [(user, options)] = user.items()
            except AttributeError:
                privileges = None
            else:
                privileges = GroupPrivileges(privileges=options['privileges'])

            user_info = users[user]
            try:
                admin_client.add_group_user(group_id, user_info.id,
                                            privileges=privileges)
            except ApiException as ex:
                raise RuntimeError('failed to add {user} user to {group} group '
                                   'because of: \n{status}: {reason}'
                                   ''.format(user=user_info.username,
                                             group=group_name,
                                             status=ex.status,
                                             reason=ex.reason))


def _create_group(owner, group_name, zone_host):
    oz_client = get_oz_user_api(owner.username, owner.password, zone_host)
    oz_client.create_group_for_user(Group(name=group_name))

    for group_id in oz_client.list_user_groups().groups:
        group = oz_client.get_user_group(group_id)
        if group.name == group_name:
            return group.group_id
