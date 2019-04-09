#!/usr/bin/env escript
%%! -name getregistrationtoken@test

%% Ensures a user with given Username exists and generates
%% a provider registration token for them
main([OZ_Node, Cookie, Username]) ->
    OZ = list_to_atom(OZ_Node),
    erlang:set_cookie(OZ, list_to_atom(Cookie)),

    {ok, UserId} = rpc:call(OZ, user_logic, acquire_onepanel_user,
        [<<>>, list_to_binary(Username), <<"admin">>]),
    UserClient = rpc:call(OZ, entity_logic, user_client, [UserId]),
    {ok, RegistrationToken} = rpc:call(OZ, user_logic,
        create_provider_registration_token, [UserClient, UserId]),
    {ok, SerializedToken} = rpc:call(OZ, onedata_macaroons, serialize, [RegistrationToken]),

    io:format(SerializedToken).