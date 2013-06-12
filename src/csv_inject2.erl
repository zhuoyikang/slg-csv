-module(csv_inject2).
-compile([export_all]).

inject_vip_exp(Vip, P) ->
  io:format("vip ~p ~p~n", [Vip, P]),
  Vip.
