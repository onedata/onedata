#!/usr/bin/env escript
%%! -name getmacaroon@test

% Value in DB meaning that alias is not set.
% Empty list, must be used as a list not binary so JS view will work correctly
-define(EMPTY_ALIAS, <<"">>).

% Regexp to validate aliases - at least 5 alphanumeric chars
-define(ALIAS_VALIDATION_REGEXP, <<"^[a-z0-9]{5,}$">>).

% String that will be put in front of uuid when a user does not have an alias set.
% Aliases are not allowed to start with this string.
-define(NO_ALIAS_UUID_PREFIX, "uuid_").

%% This record defines a user and is handled as a database document
-record(onedata_user, {
    name = <<"">> :: binary(),
    alias = ?EMPTY_ALIAS :: binary(),
    email_list = [] :: [binary()],
    connected_accounts = [],
    spaces = [] :: [SpaceId :: binary()],
    space_names = #{} :: #{SpaceId :: binary() => SpaceName :: binary()},
    default_space :: binary() | undefined,
    groups = [] :: [GroupId :: binary()],
    first_space_support_token = <<"">> :: binary(),
    % This allows to remember the provider which was selected for user, so DNS knows where to redirect
    default_provider = undefined :: binary() | undefined,
    % This allows to remember to which provider user is being redirected.
    % It is needed in DNS so it knows where to redirect.
    chosen_provider = undefined :: binary() | undefined,
    % List of user's client tokens
    client_tokens = [] :: [binary()]
}).

-record(document, {
    key :: datastore:ext_key(),
    %% holds revision
    %% or revision history (in changes stream)
    rev :: term(),
    %% if record has been deleted  (in changes stream)
    deleted = false :: boolean(),
    value :: datastore:value(),
    links :: term()
}).


main([OZNode, OZCookie, OPNode, OPCookie, Email, ProviderId]) ->
    OZ = list_to_atom(OZNode),
    OP = list_to_atom(OPNode),
    true = erlang:set_cookie(OZ, list_to_atom(OZCookie)),
    true = erlang:set_cookie(OP, list_to_atom(OPCookie)),
        {ok, #document{value=#onedata_user{name=UserID}}} = rpc:call(OZ, user_logic, get_user_doc, [{email, list_to_binary(Email)}]),
    SrlzdMacaroon = rpc:call(OZ, auth_logic, gen_token, [UserID, list_to_binary(ProviderId)]),
    {ok, Macaroon} = rpc:call(OZ, macaroon, deserialize, [SrlzdMacaroon]),
    Caveats = rpc:call(OZ, macaroon, third_party_caveats, [Macaroon]),

    DischMacaroons = lists:map(
        fun({_, CaveatId}) ->
            {ok, SrlzdDM} = rpc:call(OP, oz_users, authorize, [CaveatId]),
            {ok, DM} = rpc:call(OP, macaroon, deserialize, [SrlzdDM]),
            DM
        end, Caveats),

    BoundMacaroons = lists:map(
        fun(DM) ->
            BDM = rpc:call(OZ, macaroon, prepare_for_request, [Macaroon, DM]),
            {ok, SerializedBDM} = rpc:call(OZ, macaroon, serialize, [BDM]),
            SerializedBDM
        end, DischMacaroons),
    % Bound discharge macaroons are sent in one header,
    % separated by spaces.
    {ok, SerializedMacaroon} = rpc:call(OZ, macaroon, serialize, [Macaroon]),
    BoundMacaroonsValue = rpc:call(OZ, str_utils, join_binary, [BoundMacaroons, <<" ">>]),

    Headers = rpc:call(OZ, json_utils, encode, [[
        {<<"macaroon">>, SerializedMacaroon},
        {<<"discharge-macaroons">>, BoundMacaroonsValue}
    ]]),
    io:format("~s", [Headers]).
