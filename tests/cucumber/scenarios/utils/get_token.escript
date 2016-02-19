#!/usr/bin/env escript
%%! -name gettoken@test -setcookie cookie3

main([GR_Node, UID]) ->
  try
    Token = rpc:call(list_to_atom(GR_Node), auth_logic, gen_token, [list_to_binary(UID)]),
    io:format(Token)
  catch
      _T:M -> io:format("~p~n", [M])
  end.