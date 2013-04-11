-module(node_test).
-export([register_func/0, get_func_pid/0]).
-import(fib, [fibo/1]).

register_func() ->
    global:register_name(fibname, spawn(fib, fibo, [9])).

get_func_pid() ->
    global:whereis_name(fibname).
