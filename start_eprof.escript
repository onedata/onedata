#!/usr/bin/env escript
%%! -name parse@parse -setcookie cookie2

main([OpNodeStr, ProfilingLog, Command | Args]) ->
    OpNode = list_to_atom(OpNodeStr),

    Self = self(),
    Eprof = fun F() ->
        receive
            start_eprof ->
                {ok, _} = eprof:start(),
                RegisteredWithEprof = erlang:registered(),
                Registered = [Name || Name <- RegisteredWithEprof, Name =/= eprof],
                profiling = eprof:start_profiling(Registered),
                Self ! started,
                F();
            stop_eprof ->
                timer:sleep(timer:seconds(1)),
                profiling_stopped = eprof:stop_profiling(),
                ok = eprof:log(ProfilingLog),
                ok = eprof:analyze(procs),
                stopped = eprof:stop(),
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

    {profiler, OpNode} ! stop_eprof,
    receive
        stopped -> ok
    end.