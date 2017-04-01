-module(chat_server_ui).

-export([stop/0, print_users/0, start/0]).

start() ->
	register(messenger, spawn(chat_server, server, [])).

print_users() ->
	messenger ! print_users.

stop() ->
	messenger ! finish.
