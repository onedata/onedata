"""Steps for spaces management using REST API.
"""

import yaml

from pytest_bdd import given, parsers, then
from tests.gui.utils.generic import parse_seq
from tests.gui.utils.onezone_client.configuration import Configuration as Conf_OZ
# from swagger_common import login_to_op_with_given_username_and_password, \
#     login_to_oz_with_given_username_and_password
from tests.gui.utils.onezone_client import Space, UserApi, SpaceApi
from tests.gui.utils.onepanel_client import OneproviderApi, SpaceSupportRequest
from parse import parse


__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


@given(parsers.parse('initial spaces configuration:\n{configuration}'))
def created_spaces(configuration):
    a = yaml.load(configuration)
    b = a
    return a


# def _get_id_of_users_space_with_given_name(space_name, user_api):
#     spaces = user_api.get_user_spaces().spaces
#     space_id = ''
#     for space in spaces:
#         space = user_api.get_user_space(space)
#         if space.name == space_name:
#             space_id = space.space_id
#     return space_id
#
#
# # step example: (each create_unsupported_space_record have to be separated with semicolon)
# # created unsupported spaces: [space2, space3] for user of browser1; [space4, space5] for
# # user of browser2
# @given(parsers.parse('created unsupported spaces: {create_unsupported_space_records}'))
# def created_spaces(create_unsupported_space_records, tmp_memory, onezone_rest_url):
#
#     users_spaces = {}
#     records = list_parser(create_unsupported_space_records, separator=';')
#     for record in records:
#         parsed = parse('{spaces_names} for user of {browser}', record)
#         browser = parsed['browser']
#         spaces_names = list_parser(parsed['spaces_names'])
#         username = tmp_memory[browser]['username']
#         Conf_OZ().verify_ssl = False
#         user_client = login_to_oz_with_given_username_and_password(username,
#                                                                    'Password1',
#                                                                    onezone_rest_url)
#         user_api = UserApi(user_client)
#
#         for space_name in spaces_names:
#             new_space = Space()
#             new_space.name = space_name
#             user_api.create_user_space(new_space)
#             space_id = _get_id_of_users_space_with_given_name(space_name, user_api)
#             if username in users_spaces:
#                 users_spaces[username][space_name] = space_id
#             else:
#                 users_spaces[username] = {space_name: space_id}
#     return users_spaces
#
#
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
#
#
# @then(parsers.parse('user of {browser_id} belongs to the new space with given name in database'))
# def check_if_user_belongs_to_space_in_database(browser_id, tmp_memory, onezone_rest_url,
#                                                name_string):
#     username = tmp_memory[browser_id]['username']
#     Conf_OZ().verify_ssl = False
#
#     user_client = login_to_oz_with_given_username_and_password(username,
#                                                                'Password1',
#                                                                onezone_rest_url)
#
#     user_api = UserApi(user_client)
#     spaces = user_api.get_user_spaces().spaces
#     spaces = [user_api.get_user_space(x).name for x in spaces]
#     assert name_string in spaces
#
#
# @then(parsers.parse('user\'s of {browser_id} home space has changed to "{space_name}" in database'))
# def check_if_users_home_space_has_changed_in_database(browser_id, tmp_memory, onezone_rest_url,
#                                                       created_spaces, space_name):
#     username = tmp_memory[browser_id]['username']
#     Conf_OZ().verify_ssl = False
#
#     user_client = login_to_oz_with_given_username_and_password(username,
#                                                                'Password1',
#                                                                onezone_rest_url)
#
#     user_api = UserApi(user_client)
#     home_space = user_api.get_user_default_space()
#     assert home_space.space_id == created_spaces[username][space_name]
#
#
# @then(parsers.parse('user of {browser_id} doesn\'t belong to space "{space_name}" in database'))
# def chceck_if_user_doesnt_belong_to_space_in_database(browser_id, tmp_memory, onezone_rest_url,
#                                                       created_spaces, space_name):
#     username = tmp_memory[browser_id]['username']
#     Conf_OZ().verify_ssl = False
#
#     user_client = login_to_oz_with_given_username_and_password(username,
#                                                                'Password1',
#                                                                onezone_rest_url)
#
#     user_api = UserApi(user_client)
#     spaces = user_api.get_user_spaces().spaces
#     assert created_spaces[username][space_name] not in spaces
#
#
# @then(parsers.parse('user\'s of {browser_id} space "{space_name}" has been renamed to "{new_space_name}" in database'))
# def check_if_users_space_has_been_renamed_in_database(browser_id, tmp_memory, onezone_rest_url,
#                                                       created_spaces_supported_by_provider,
#                                                       new_space_name, space_name):
#     username = tmp_memory[browser_id]['username']
#     Conf_OZ().verify_ssl = False
#
#     user_client = login_to_oz_with_given_username_and_password(username,
#                                                                'Password1',
#                                                                onezone_rest_url)
#
#     user_api = UserApi(user_client)
#     assert user_api.get_user_space(created_spaces_supported_by_provider[username][space_name]).name == new_space_name
