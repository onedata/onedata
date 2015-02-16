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

-export([response_mocks/0]).

% This function should return a list of mappings {Port, Path} -> {Response}.
% If a request is performed on certain port and certain path, the response will be returned.
response_mocks() -> [
    % First type of response can be static binary. It is returned every time the endpoint is requested.
    % #mock_resp has default values for code, content_type and headers. They can be easily overriden,
    % but in most cases it's enough to specify just the 'body' field.
    #mock_resp_mapping{port = 443, path = <<"/test1">>, response = #mock_resp{code = 206, content_type = <<"text/plain">>,
        headers = [{<<"a">>, <<"b">>}, {<<"c">>, <<"d">>}], body = <<"this is test1 endpoint">>}},

    % Second type of response can be a list of static responses. They are returned in order of the list.
    % If the end of the list is reached, it starts from the beggining again.
    #mock_resp_mapping{port = 443, path = <<"/test2">>, response = [
        #mock_resp{body = <<"lorem ipsum">>},
        #mock_resp{body = <<"dolor sit amet,">>},
        #mock_resp{body = <<"consectetur adipiscing elit.">>}
    ]},

    % Third type are dynamically generated responses. To define such a response, a two argument function must
    % be provided. Args are Req (cowboy req record) and State (custom state of the mapping, passed between requests).
    % Returned value is a tuple containing #mock_resp record and new state.
    #mock_resp_mapping{port = 443, path = <<"/test_with_state">>,
        response = fun(_Req, State) ->
            R = #mock_resp{body = <<"Counter: ", (integer_to_binary(State))/binary>>},
            {R, State + 1}
        end,
        initial_state = 0},

    % If types inside the mapping are not obided by, the appmock will return a 500 Internal server error response.
    #mock_resp_mapping{port = 443, path = <<"/test3">>, response = some_rubbish_that_will_cause_appmock_to_crash}
].