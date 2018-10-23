#!/usr/bin/env escript
%%! -name gettoken@test

main([OZ_Node, UID, Cookie]) ->
    OZ = list_to_atom(OZ_Node),
    erlang:set_cookie(OZ, list_to_atom(Cookie)),
    Token = rpc:call(OZ, auth_tokens, gen_token, [list_to_binary(UID)]),
    io:format(Token).