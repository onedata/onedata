#!/usr/bin/env escript
%%! -name parse@parse -setcookie cookie2

-define(PROCESS_INFO_KEYS, [
    registered_name,
    current_function,
    current_location,
    current_stacktrace
]).

main([OpNodeStr, PidStr]) ->
    OpNode = list_to_atom(OpNodeStr),
    case get_registered_name(OpNode, PidStr) of
        undefined -> io:format("undefined");
        InfoList -> lists:foreach(fun(Key) ->
            Info = proplists:get_value(Key, InfoList),
            io:format("~p: ~p~n", [Key, Info])
        end, ?PROCESS_INFO_KEYS)
    end.


get_registered_name(Node, PidStr) ->

    GetName = fun(Pid) ->
        receive
            {get_name, Parent} ->

                Name = erlang:process_info(list_to_pid(Pid), ?PROCESS_INFO_KEYS), %of

%%                    {registered_name, RegisteredName} -> RegisteredName;
%%                    undefined -> undefined;
%%                    [] -> erlang:process_info(list_to_pid(Pid))
%%                end,
                Parent ! {self(), Name}
        end
    end,
    Pid = spawn_link(Node, erlang, apply, [GetName, [PidStr]]),
    Pid ! {get_name, self()},

    receive
        {Pid, ProcessInfo} ->
            ProcessInfo
%%            case ProcessInfo of
%%                undefined -> undefined;
%%                List -> proplists
%%                [
%%                    {registered_name, RegisteredName},
%%                    {current_function, CurrentFunction},
%%                    {current_location, CurrentLocation},
%%                    {current_stacktrace, CurrentStacktrace}
%%                ]
%%            end
    end.