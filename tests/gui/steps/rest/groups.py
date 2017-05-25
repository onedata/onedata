"""Steps for groups management using REST API.
"""

import yaml

from tests.gui.utils.onezone_client import Group, GroupPrivileges
from pytest_bdd import given, parsers

from .common import get_oz_user_api, get_oz_group_api


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


def _get_id_of_users_group_with_given_name(group_name, client):
    for group_id in client.list_user_groups().groups:
        group = client.get_user_group(group_id)
        if group.name == group_name:
            return group.group_id


@given(parsers.parse('initial groups configuration in "{service}" '
                     'Onezone service:\n{config}'))
def create_groups_according_to_given_configuration(config, service,
                                                   admin_credentials,
                                                   users, hosts, groups):
    """Create and configure groups according to given config.

    Config format given in yaml is as follow:

        group_name_1:
            owner: user_name                ---> currently we identify user account with concrete
                                                 browser so user_name == browser_id
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
            owner: browser2
            users:
                - browser3:
                    privileges:
                        - group_invite_user
                        - group_remove_user
                - browser4
    """
    host = hosts['onezone'][service]
    for name, description in yaml.load(config).items():
        owner_cred = users[description['owner']]
        user_client = get_oz_user_api(owner_cred.username,
                                      owner_cred.password, host)
        user_client.create_group_for_user(Group(name=name))
        group_id = _get_id_of_users_group_with_given_name(name, user_client)
        groups[name] = group_id

        admin_client = get_oz_group_api(admin_credentials.username,
                                        admin_credentials.password, host)
        for user in description['users']:
            try:
                [(user, options)] = user.items()
            except AttributeError:
                admin_client.add_group_user(group_id, users[user].id)
            else:
                privileges = GroupPrivileges(privileges=options['privileges'])
                admin_client.add_group_user(group_id, users[user].id,
                                            privileges=privileges)
