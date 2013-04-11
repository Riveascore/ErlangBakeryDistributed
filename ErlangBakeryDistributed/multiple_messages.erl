-module(multiple_messages).
-export([multiple_messages/0, one_message/0]).

multiple_messages() ->
    receive
