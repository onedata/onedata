#!/usr/bin/env escript
%%! -name gettoken@test -setcookie cookie3

main([GR_Node, UID]) ->
  % io:format("Hello"),
  % set_up_net_kernel(),
  % io:format("NODE: ~p~n", [node()]),
  % GR_Address = list_to_atom("gr@" ++ GR_Node),
  % io:format("GR ADDRESS: ~p~n", [GR_Address]),
  % io:format(atom_to_list(net_adm:ping(list_to_atom(GR_Node))) ++ "~n"),
  % io:format("NODES: ~p~n", [nodes()]),
  try
    Token = rpc:call(list_to_atom(GR_Node), auth_logic, gen_auth_code, [list_to_binary(UID)]),
    io:format(Token)
  catch
      _T:M -> io:format("~p~n", [M])
  end.