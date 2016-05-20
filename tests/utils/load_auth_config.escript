#!/usr/bin/env escript
%%! -name loadauthconfig@test

main([OZ_Node, Cookie]) ->
    OZ = list_to_atom(OZ_Node),
    true = erlang:set_cookie(OZ, list_to_atom(Cookie)),
    Ret = rpc:call(OZ, auth_config, load_auth_config, []),
    Ret = ok.
