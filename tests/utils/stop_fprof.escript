#!/usr/bin/env escript
%%! -name parse@parse -setcookie cookie2

main([OpNodeStr]) ->
    OpNode = list_to_atom(OpNodeStr),

    ok = rpc:call(OpNode, fprof, trace, [[stop]]),
    ok = rpc:call(OpNode, fprof, profile, []),
    ok = rpc:call(OpNode, fprof, analyse, [{dest, []}]).
