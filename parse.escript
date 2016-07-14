#!/usr/bin/env escript
%%! -name parse@parse

main([FileName]) ->
    Lines = readlines(FileName),
%%    io:format("~p", [Lines]),
    [H | T] = Lines,
    io:format("~p", [string:strip(H, "\n")]).




readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> get_all_lines(Device, Accum ++ [Line])
    end.


