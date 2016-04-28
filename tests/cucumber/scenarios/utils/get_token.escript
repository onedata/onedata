#!/usr/bin/env escript
%%! -name gettoken@test

main([OZ_Node, UID, Cookie]) ->
    erlang:set_cookie(list_to_atom(OZ_Node), list_to_atom(Cookie)),
    Token = rpc:call(list_to_atom(OZ_Node), auth_logic, gen_token, [list_to_binary(UID)]),
    io:format(Token).