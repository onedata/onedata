"""Steps for spaces management using REST API.
"""

import yaml

from tests.gui.utils.onezone_client import SpaceCreateRequest, SpacePrivileges
from pytest_bdd import given, parsers

from .common import get_oz_user_api, get_oz_space_api


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


def _get_id_of_users_space_with_given_name(space_name, client):
    for space_id in client.list_user_spaces().spaces:
        space = client.get_user_space(space_id)
        if space.name == space_name:
            return space.space_id


@given(parsers.parse('initial spaces configuration in "{service}" '
                     'Onezone service:\n{config}'))
def create_spaces_according_to_given_configuration(config, service,
                                                   admin_credentials, hosts,
                                                   users, groups, spaces):
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
            [groups]:                       ---> optional
                - group_name_1:
                    [privileges]:           ---> optional
                        - privilege_1
                        - privilege_2
                - group_name_2
            [providers]:                    ---> optional
                - provider_name_1:
                    storage: path
                    size: size in bits
        space_name_2:
            ...

    Example configuration:

       space1:
           owner: browser2
           users:
               - browser2
               - browser3
           groups:
               - group1
           providers:
               - p1:
                   storage: /mnt/st1
                   size: 1000000000
    """
    host = hosts['onezone'][service]
    for name, description in yaml.load(config).items():
        owner_cred = users[description['owner']]
        user_client = get_oz_user_api(owner_cred.username,
                                      owner_cred.password, host)
        user_client.create_user_space(SpaceCreateRequest(name=name))
        space_id = _get_id_of_users_space_with_given_name(name, user_client)
        spaces[name] = space_id

        admin_client = get_oz_space_api(admin_credentials.username,
                                        admin_credentials.password, host)
        for user in description['users']:
            try:
                [(user, options)] = user.items()
            except AttributeError:
                admin_client.add_user_to_space(space_id, users[user].id)
            else:
                privileges = SpacePrivileges(privileges=options['privileges'])
                admin_client.add_user_to_space(space_id, users[user].id,
                                               privileges=privileges)
        for group in description['groups']:
            try:
                [(group, options)] = group.items()
            except AttributeError:
                admin_client.add_group_to_space(space_id, groups[group])
            else:
                privileges = SpacePrivileges(privileges=options['privileges'])
                admin_client.add_group_to_space(space_id, groups[group],
                                                privileges=privileges)


# # step example: (each create_supported_space_record have to be separated with semicolon)
# # created supported spaces: [space1, space2] supported by the first provider for user of browser1;
# # [space3, space4] supported by the first provider for user of browser2
# @given(parsers.parse('created supported spaces: {create_supported_space_records}'))
# def created_spaces_supported_by_provider(create_supported_space_records,
#                                          op_panel_url, onezone_rest_url,
#                                          tmp_memory, admin_credentials):
#
#     users_spaces = {}
#     records = list_parser(create_supported_space_records, separator=';')
#     for record in records:
#         parsed = parse('{spaces_names} supported by the first provider for user of {browser}', record)
#         browser = parsed['browser']
#         spaces_names = list_parser(parsed['spaces_names'])
#         Conf_OZ().verify_ssl = False
#         username = tmp_memory[browser]['username']
#         password = tmp_memory[browser]['password']
#         user_client = login_to_oz_with_given_username_and_password(username,
#                                                                    password,
#                                                                    onezone_rest_url)
#         oneprovider_client = login_to_op_with_given_username_and_password(admin_credentials['username'],
#                                                                           admin_credentials['password'],
#                                                                           op_panel_url)
#
#         # Create needed API's
#         user_api = UserApi(user_client)
#         space_api = SpaceApi(user_client)
#         oneprovider_api = OneproviderApi(oneprovider_client)
#
#         for space_name in spaces_names:
#             new_space = Space()
#             new_space.name = space_name
#             user_api.create_user_space(new_space)
#
#             # Get id of space1
#             space_to_support_id = _get_id_of_users_space_with_given_name(space_name, user_api)
#
#             # Get space support token
#             space_support_token = space_api.get_space_provider_token(space_to_support_id)
#             space_support_request = SpaceSupportRequest(token=space_support_token.token,
#                                                         size=1024,
#                                                         storage_name='NFS')
#
#             # Support space
#             oneprovider_api.put_provider_spaces(space_support_request=space_support_request)
#
#             if username in users_spaces:
#                 users_spaces[username][space_name] = space_to_support_id
#             else:
#                 users_spaces[username] = {space_name: space_to_support_id}
#     return users_spaces
