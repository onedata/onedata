#!/usr/bin/env escript
%%! -name gettoken@test
%%-setcookie cookie3

main([OZ_Node, UID, Cookie]) ->
  try
    erlang:set_cookie(OZ_Node, Cookie),
    Token = rpc:call(list_to_atom(OZ_Node), auth_logic, gen_token, [list_to_binary(UID)]),
    io:format(Token)
  catch
      _T:M -> io:format("~p~n", [M])
  end.