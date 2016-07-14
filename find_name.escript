#!/usr/bin/env escript
%%! -name parse@parse -setcookie cookie2

main([OpNodeStr, PidStr]) ->
    OpNode = list_to_atom(OpNodeStr),
    io:format("~p", [get_registered_name(OpNode, PidStr)]).


get_registered_name(Node, PidStr) ->

    GetName = fun(Pid) ->
        receive
            {get_name, Parent} ->

                Name = erlang:process_info(list_to_pid(Pid), [registered_name, current_function,
                current_location, current_stacktrace]), %of

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
        {Pid, RegisteredName} -> RegisteredName
    end.