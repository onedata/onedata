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
rest_mocks() -> [].


% This function should return a list of #tcp_server_mock{} records. A TCP server will be
% started for each such record. Later on in remote control, the port number is used to uniquely
% identify a server.
tcp_server_mocks() -> [
    #tcp_server_mock{
        port = 5555,
        ssl = true,
        packet = raw,
        type = history
    },
    #tcp_server_mock{
        port = 6666,
        ssl = true,
        packet = raw,
        type = counter
    }
].