#!/usr/bin/env escript
%%! -name getregistrationtoken@test

%% Ensures a user with given Username exists and generates
%% a provider registration token for them
main([OZ_Node, Cookie, Username]) ->
    OZ = list_to_atom(OZ_Node),
    erlang:set_cookie(OZ, list_to_atom(Cookie)),
    UsernameBin = list_to_binary(Username),

    RootClient = rpc:call(OZ, entity_logic,root_client, []),
    {ok, UserId} = rpc:call(OZ, user_logic, create,
        [RootClient, #{<<"name">> => UsernameBin, <<"alias">> => UsernameBin}]),

    UserClient = rpc:call(OZ, entity_logic, user_client, [UserId]),
    {ok, RegistrationToken} = rpc:call(OZ, user_logic,
        create_provider_registration_token, [UserClient, UserId]),
    {ok, SerializedToken} = rpc:call(OZ, onedata_macaroons, serialize, [RegistrationToken]),

    io:format("~s", [SerializedToken]).
