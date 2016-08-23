"""Module implements pytest-bdd steps for space management via REST
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests.utils.space_utils import unsupport_space
from tests.utils.acceptance_utils import list_parser
from tests.utils.space_utils import (create_space, support_space,
                                     request_support, invite_to_space,
                                     join_space, remove_user, delete_space,
                                     assign_privileges)

from pytest_bdd import parsers, then, when


@when(parsers.parse('{user} creates spaces {spaces}'))
def spaces_creation(user, spaces, onedata_environment, context):
    spaces = list_parser(spaces)
    user = context.get_user(user)
    for space in spaces:
        space_id = create_space(user, space)
        user.spaces.update({space: space_id})
        user.created_spaces.update({space: space_id})


@when(parsers.parse('{user} gets token to support spaces {spaces}'))
def request_spaces_support(user, spaces, onedata_environment, context):
    spaces = list_parser(spaces)
    user = context.get_user(user)
    for space in spaces:
        token = request_support(user, space)
        user.tokens['support'].update({space: token})


@when(parsers.parse('{space} is supported with {size} MB for {user} by provider {provider_id}'))
def support_spaces(space, user, provider_id, size, env_description_file,
                   onedata_environment, context, providers):
    user = context.get_user(user)
    provider = providers[provider_id]
    size = 1024 * 1024 * int(size)
    space_id = support_space(user, space, provider, size, env_description_file, onedata_environment)
    provider.spaces.update({space: space_id})


@when(parsers.parse('{user1} invites {user2} to space {space}'))
@then(parsers.parse('{user1} invites {user2} to space {space}'))
def space_invitation(user1, user2, space, context, onedata_environment):
    user1 = context.get_user(user1)
    user2 = context.get_user(user2)
    token = invite_to_space(user1, user2, space)
    user2.tokens['space_invite'].update({space: token})


@when(parsers.parse('{user} joins space {space}'))
def space_join(user, space, context, onedata_environment):
    user = context.get_user(user)
    join_space(user, space)


@when(parsers.parse('{user1} removes {user2} from space {space}'))
def removing_user_from_space(user1, user2, space, context, onedata_environment):
    user1 = context.get_user(user1)
    user2 = context.get_user(user2)
    remove_user(user1, user2, space)


@when(parsers.parse('{user} deletes space {space}'))
def deleting_space(user, space, context, onedata_environment):
    user = context.get_user(user)
    delete_space(user, space)
    del user.spaces[space]
    if space in user.created_spaces.keys():
        del user.created_spaces[space]


@when(parsers.parse('{user1} assigns {user2} privileges {privileges} for space {space}'))
def assigning_privileges(user1, user2, privileges, space, context):
    user1 = context.get_user(user1)
    user2 = context.get_user(user2)
    privileges = list_parser(privileges)
    assign_privileges(user1, user2, privileges, space)


@when(parsers.parse('provider {provider_id} unsupports space {space}'))
def stop_supporting_space(provider_id, space, providers, context):

    provider = providers[provider_id]
    unsupport_space(provider, space)
    del provider.spaces[space]
