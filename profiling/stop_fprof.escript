#!/usr/bin/env escript
%%! -name parse@parse -setcookie cookie2

main([OpNodeStr, ProfilingLog]) ->
    OpNode = list_to_atom(OpNodeStr),

    rpc:call(OpNode, fprof, trace, [[stop]]),
    rpc:call(OpNode, fprof, profile, []),
    rpc:call(OpNode, fprof, analyse, [{dest, ProfilingLog}]).
