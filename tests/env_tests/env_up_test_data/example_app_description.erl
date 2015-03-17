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
-module(example_app_description).
-behaviour(mock_app_description_behaviour).

-include_lib("appmock/include/appmock.hrl").
-include_lib("ctool/include/logging.hrl").

-export([rest_mocks/0, tcp_server_mocks/0]).

% This function should return a list of #rest_mock{} records,
% which in essence hold mappings {Port, Path} -> {Response}.
% If a request is performed on certain port and certain path, the response will be returned.
rest_mocks() -> [
    % First type of response can be static binary. It is returned every time the endpoint is requested.
    % #mock_resp has default values for code, content_type and headers. They can be easily overriden,
    % but in most cases it's enough to specify just the 'body' field.
    % Path can be any binary compatible with cowboy's router syntax:
    % http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing/ - see matching paths.
    #rest_mock{port = 8080, path = <<"/test1/[:binding]">>, response = #rest_response{code = 206, content_type = <<"text/plain">>,
        headers = [{<<"a">>, <<"b">>}, {<<"c">>, <<"d">>}], body = <<"this is test1 endpoint">>}},

    % Second type of response can be a list of static responses. They are returned in order of the list.
    % If the end of the list is reached, it starts from the beggining again.
    #rest_mock{port = 8080, path = <<"/test2">>, response = [
        #rest_response{body = <<"lorem ipsum">>},
        #rest_response{body = <<"dolor sit amet,">>},
        #rest_response{body = <<"consectetur adipiscing elit.">>}
    ]},

    % Third type are dynamically generated responses. To define such a response, a two argument function must
    % be provided. Args are Req (cowboy req record) and State (custom state of the mapping, passed between requests).
    % Returned value is a tuple containing #mock_resp record and new state.
    #rest_mock{port = 9090, path = <<"/test_with_state">>,
        response = fun(_Req, State) ->
            R = #rest_response{body = <<"Counter: ", (integer_to_binary(State))/binary>>},
            {R, State + 1}
        end,
        initial_state = 0},

    % If types inside the mapping are not obided by, the appmock will return a 500 Internal server error response.
    #rest_mock{port = 8080, path = <<"/test3">>, response = some_rubbish_that_will_cause_appmock_to_crash}

    % There is a cowboy_req facade module for convenience, called req.
    % It contains most useful functions to get information about incoming requests.
    % They can be used inside response functions.
    #rest_mock{port = 443, path = <<"/[:binding/[...]]">>,
        response = fun(Req, _State) ->
            Headers = req:headers(Req),
            ContentType = req:header(<<"content-type">>, Req),
            Host = req:host(Req),
            Peer = req:peer(Req),
            Path = req:path(Req),
            Binding = req:binding(binding, Req),
            Body = req:body(Req),
            PostParams = req:post_params(Req),
            ResponseBody = gui_str:format_bin(
                "Your request contained:~n" ++
                    "Host:        ~s~n" ++
                    "Path:        ~s~n" ++
                    "Binding:     ~p~n" ++
                    "Peer:        ~p~n" ++
                    "ContentType: ~p~n" ++
                    "Headers:     ~p~n" ++
                    "Body:        ~p~n" ++
                    "PostParams:  ~p~n",
                [Host, Path, Binding, Peer, ContentType, Headers, Body, PostParams]),
            {#rest_response{body = ResponseBody, content_type = <<"text/plain">>}, whatever}
        end,
        initial_state = whatever}
].


% This function should return a list of #tcp_server_mock{} records. A TCP server will be
% started for each such record. Later on in remote control, the port number is used to uniquely
% identify a server.
tcp_server_mocks() -> [
    #tcp_server_mock{
        port = 5555,
        ssl = true
    }
].