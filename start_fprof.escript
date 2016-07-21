#!/usr/bin/env escript
%%! -name parse@parse -setcookie cookie2

main([OpNodeStr, ProfilingLog, Command | Args]) ->
    OpNode = list_to_atom(OpNodeStr),

    Self = self(),
    Eprof = fun F() ->
        receive
            start_eprof ->
                ok = fprof:trace([start, verbose, {procs, all}]),
                Self ! started,
                F();
            stop_eprof ->
                ok = fprof:trace([stop]),
                ok = fprof:profile(),
                ok = fprof:analyse({dest, [ProfilingLog]}),
                Self ! stopped
        end
    end,
    FullCommand = string:join([Command | Args], " "),

    Pid = spawn_link(OpNode, Eprof),
    rpc:call(OpNode, erlang, register, [profiler, Pid]),
    {profiler, OpNode} ! start_eprof,

    receive
        started -> ok
    end,

    os:cmd(FullCommand),

    timer:sleep(timer:seconds(15)),

    {profiler, OpNode} ! stop_eprof,
    receive
        stopped -> ok
    end.