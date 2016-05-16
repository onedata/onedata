#!/usr/bin/env escript
%%! -name gettoken@test

main([OZ_Node, Cookie]) ->
    true = erlang:set_cookie(list_to_atom(OZ_Node), list_to_atom(Cookie)),
    Ret = rpc:call(list_to_atom(OZ_Node), auth_config, load_auth_config, []),
    Ret = ok.
