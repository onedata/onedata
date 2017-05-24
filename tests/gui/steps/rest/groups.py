"""Steps for groups management using REST API.
"""

import yaml

# from swagger_common import login_to_oz_with_given_username_and_password
from tests.gui.utils.generic import parse_seq
from tests.gui.utils.onezone_client import UserApi, Group
from pytest_bdd import given, parsers, then
from tests.gui.utils.onezone_client.configuration import Configuration as Conf_OZ
from parse import parse


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@given(parsers.parse('initial groups configuration in "{service}" '
                     'Onezone service:\n{config}'))
def create_groups_according_to_given_configuration(config, service,
                                                   admin_credentials,
                                                   users, hosts):
    host = hosts['onezone'][service]
    for name, description in yaml.load(config).items():
        pass
        for user in description['users']:
            credentials = users[user]


def _get_id_of_users_group_with_given_name(group_name, user_api):
    groups = user_api.get_user_groups(callback=None).groups
    group_id = ''
    for group in groups:
        group = user_api.get_user_group(group)
        if group.name == group_name:
            group_id = group.group_id
    return group_id


@given(parsers.parse('created groups: {create_group_records}'))
def created_groups(create_group_records, tmp_memory,
                   onezone_rest_url):

    users_groups = {}
    records = parse_seq(create_group_records, separator=';')
    for record in records:
        parsed = parse('{groups_names} for user of {browser}', record)
        browser = parsed['browser']
        groups_names = parse_seq(parsed['groups_names'])

        Conf_OZ().verify_ssl = False
        username = tmp_memory[browser]['username']
        password = tmp_memory[browser]['password']
        user_client = login_to_oz_with_given_username_and_password(username,
                                                                   password,
                                                                   onezone_rest_url)

        user_api = UserApi(user_client)

        for group_name in groups_names:
            new_group = Group()
            new_group.name = group_name
            new_group.type = 'role'
            user_api.create_group_for_user(new_group)
            group_id = _get_id_of_users_group_with_given_name(group_name, user_api)
            if username in users_groups:
                users_groups[username][group_name] = group_id
            else:
                users_groups[username] = {group_name: group_id}

    return users_groups

#
#
# # step example: (each create_group_record have to be separated with semicolon)
# # created groups: [group1, group2] for user of browser1; [group3, group4] for user of browser2
#
#
# @then(parsers.parse('user of {browser_id} belongs to the new group with given name in database'))
# def check_if_user_belongs_to_group_in_database(browser_id, tmp_memory, name_string,
#                                                onezone_rest_url):
#     username = tmp_memory[browser_id]['username']
#     Conf_OZ().verify_ssl = False
#
#     user_client = login_to_oz_with_given_username_and_password(username,
#                                                                'Password1',
#                                                                onezone_rest_url)
#
#     user_api = UserApi(user_client)
#     groups = user_api.get_user_groups().groups
#     groups = [user_api.get_user_group(x).name for x in groups]
#     assert name_string in groups
#
#
# @then(parsers.parse('user\'s of {browser_id} group "{group_name}" has been renamed to "{new_group_name}" in database'))
# def check_if_users_group_has_been_renamed_in_database(browser_id, group_name, tmp_memory,
#                                                       new_group_name, onezone_rest_url,
#                                                       created_groups):
#     username = tmp_memory[browser_id]['username']
#     Conf_OZ().verify_ssl = False
#     user_client = login_to_oz_with_given_username_and_password(username,
#                                                                'Password1',
#                                                                onezone_rest_url)
#
#     user_api = UserApi(user_client)
#     assert user_api.get_user_group(created_groups[username][group_name], callback=None).name == new_group_name
#
#
# @then(parsers.parse('user of {browser_id} doesn\'t belong to group "{group_name}" in database'))
# def check_if_user_doesnt_belong_to_group_in_database(browser_id, tmp_memory, onezone_rest_url,
#                                                      created_groups, group_name):
#     username = tmp_memory[browser_id]['username']
#     Conf_OZ().verify_ssl = False
#     user_client = login_to_oz_with_given_username_and_password(username,
#                                                                'Password1',
#                                                                onezone_rest_url)
#
#     user_api = UserApi(user_client)
#     groups = user_api.get_user_groups().groups
#     assert created_groups[username][group_name] not in groups
