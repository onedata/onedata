#!/usr/bin/env escript
%%! -name parse@parse -setcookie cookie2

main([OpNodeStr, ProfilingLogFile]) ->
    OpNode = list_to_atom(OpNodeStr),

%%    true = net_kernel:connect_node(OpNode),
    {profiler, OpNode} ! start_eprof.



%%    profiling_stopped = rpc:call(OpNode, eprof, stop_profiling, []),
%%    ok = rpc:call(OpNode, eprof, log, [ProfilingLogFile]),
%%    ok = rpc:call(OpNode, eprof, analyze, [procs]),
%%    stopped = rpc:call(OpNode, eprof, stop, []).
