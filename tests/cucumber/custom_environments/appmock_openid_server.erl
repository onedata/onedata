%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This is appmock of openid server.
%%% @end
%%%-------------------------------------------------------------------
-module(appmock_openid_server).
-behaviour(mock_app_description_behaviour).

-include_lib("appmock/include/appmock.hrl").
-include_lib("ctool/include/logging.hrl").

-export([rest_mocks/0, tcp_server_mocks/0]).

-define(MOCK_TOKEN, <<"mock_token">>).
-record(mocked_user, {name, login, email, uid}).
-define(MOCK_USER(Name, Login, Email, UID),
    #mocked_user{name=Name, login=Login, email=Email, uid=UID}).
-define(MOCKED_USERS, #{
    1 => ?MOCK_USER(<<"u1">>, <<"login1">>, <<"u1@mail.com">>, <<"uid1">>),
    2 => ?MOCK_USER(<<"u2">>, <<"login2">>, <<"u2@mail.com">>, <<"uid2">>),
    3 => ?MOCK_USER(<<"u3">>, <<"login3">>, <<"u3@mail.com">>, <<"uid3">>),
    4 => ?MOCK_USER(<<"u4">>, <<"login4">>, <<"u4@mail.com">>, <<"uid4">>),
    5 => ?MOCK_USER(<<"u5">>, <<"login5">>, <<"u5@mail.com">>, <<"uid5">>)
}).

% This function should return a list of #rest_mock{} records,
% which in essence hold mappings {Port, Path} -> {Response}.
% If a request is performed on certain port and certain path, the response will be returned.
rest_mocks() -> [
    #rest_mock{port = 443, path = <<"/1/oauth2/authorize">>,
        response = fun(Req, State) ->
            {RedirectionPoint, _} = cowboy_req:qs_val(<<"redirect_uri">>, Req, undefined),
            {ReqState, _} = cowboy_req:qs_val(<<"state">>, Req, undefined),
            URL = <<RedirectionPoint/binary, "?code=mockcode&state=", ReqState/binary>>,
            {#rest_response{code = 307, headers = [{<<"location">>, URL}]}, State + 1}
        end,
        initial_state = 1},


    #rest_mock{port = 443, path = <<"/1/oauth2/token">>,
        response = fun(Req, State) ->
            Proplist = req:post_params(Req),
            RedirectionPoint = proplists:get_value(<<"redirect_uri">>, Proplist),
            #mocked_user{uid=UID} = maps:get(State, ?MOCKED_USERS),
            Body = json_utils:encode([
                {<<"access_token">>, <<?MOCK_TOKEN/binary, (integer_to_binary(State))/binary >>},
                {<<"uid">>, UID}]),
            URL = <<RedirectionPoint/binary, "?code=mockcode">>,
            {#rest_response{code = 200, body = Body}, State + 1}
        end,
        initial_state = 1},

    #rest_mock{port = 443, path = <<"/1/account/info">>,
        response = fun(_Req, State) ->
            #mocked_user{name=Name, login=Login, email=Email} = maps:get(State, ?MOCKED_USERS),
            Body = json_utils:encode([
                {<<"email">>, Email},
                {<<"display_name">>, Name},
                {<<"login">>, Login}]),
            {#rest_response{code = 200, body = Body}, State + 1}
        end,
        initial_state = 1}
].



% This function should return a list of #tcp_server_mock{} records. A TCP server will be
% started for each such record. Later on in remote control, the port number is used to uniquely
% identify a server.
tcp_server_mocks() -> [
    #tcp_server_mock{
        port = 5555,
        ssl = true,
        % Erlang transport's packet option that will be passed to server initialization.
        packet = raw,
        % TCP mock can work in two modes:
        % history - it remembers exact history of incoming requests and can validate requests per message contents.
        %    This mode is slow and dedicated for content verification rather that tests with many messages.
        % counter - the endpoint will ignore the content of incoming requests and only count them.
        %    This mode is as fast as it gets.
        % NOTE: in history mode, it is also possible to check the count of all received requests.
        type = counter
    }
].