%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This is an example app description used by appmock.
%%% @end
%%%-------------------------------------------------------------------
-module(appmock_openid_server).
-behaviour(mock_app_description_behaviour).

-include_lib("appmock/include/appmock.hrl").
-include_lib("ctool/include/logging.hrl").

-export([rest_mocks/0, tcp_server_mocks/0]).

% This function should return a list of #rest_mock{} records,
% which in essence hold mappings {Port, Path} -> {Response}.
% If a request is performed on certain port and certain path, the response will be returned.
rest_mocks() -> [
    %% TODO
    #rest_mock{port = 443, path = <<"/1/oauth2/authorize">>, response =
    fun(Req, _State) ->
        {RedirectionPoint, _} = cowboy_req:qs_val(<<"redirect_uri">>, Req, undefined),
        {State, _} = cowboy_req:qs_val(<<"state">>, Req, undefined),
        URL = <<RedirectionPoint/binary, "?code=mockcode&state=", State/binary>>,
        {#rest_response{code = 307, headers = [{<<"location">>, URL}]},
            _State
        }
    end},


    #rest_mock{port = 443, path = <<"/1/oauth2/token">>, response =
    fun(Req, _State) ->
        Proplist = req:post_params(Req),
        Code = proplists:get_value(<<"code">>, Proplist),
        GrantType = proplists:get_value(<<"grant_type">>, Proplist),
        RedirectionPoint = proplists:get_value(<<"redirect_uri">>, Proplist),
        Headers = req:headers(Req),
        io:format("Your request contained:~n" ++
            "Code:        ~p~n" ++
            "GrantType: ~p~n" ++
            "RedirectionPoint:  ~p~n" ++
            "Headers:     ~p~n",
            [Code, GrantType, RedirectionPoint, Headers]),

        Body = json_utils:encode([
            {<<"access_token">>, <<"mock_access_token">>},
            {<<"uid">>, <<"mock_uid">>}]),
        URL = <<RedirectionPoint/binary, "?code=mockcode">>,
        {#rest_response{code = 200, body = Body}, _State}
    end},

    #rest_mock{port = 443, path = <<"/1/account/info">>, response =
    fun(_Req, _State) ->

        Body = json_utils:encode([
            {<<"email">>, <<"mock_email">>},
            {<<"display_name">>, <<"mock_name">>},
            {<<"login">>, <<"mock_login">>}]),
        {#rest_response{code = 200, body = Body}, _State}
    end}
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