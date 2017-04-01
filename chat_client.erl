-module(chat_client).

-export([start/1]).
-include("mess_config.hrl").
-include("mess_interface.hrl").

start(UserName) ->
	start(UserName, 5).

start(_, TriesLeft) when TriesLeft < 0 ->
	io:format("Exiting ~n",[]),
	exit(normal);
	
start(UserName, TriesLeft) ->
	SPID = {messenger, ?server_node},
	SPID ! #login_user{username = UserName, pid=self()},
	receive
		logged_in ->
			io:format("Login Successful~n", []),
			process_flag(trap_exit, true),
			client(SPID, UserName); 
		{bad_log_in, Reason} ->
			io:format("Denied login: ~w ~n",[Reason]),
			exit(normal)
	after 1000 ->
		io:format("No response from login attempt. Tries left: ~w ~n",[TriesLeft]),
		start(UserName, TriesLeft -1)
	end.

client(SPID, UserName) ->
	receive
		{recive_message, FromUserName, Message}-> 
			io:format("~w: ~w ~n", [FromUserName, Message]),
			client(SPID, UserName);
		logoff ->
			SPID ! #remove_user{username=UserName},
			io:format("~w logged off.~n", [UserName]),
			exit(normal);
		#send_message{to_username=ToUserName, message=Message}-> 
			SPID ! #send_message{from_username=UserName, to_username=ToUserName, message=Message},
			client(SPID, UserName);
                {'EXIT', _, What} ->
			io:format("Lost connect to server. Exiting: ~w ~n", [What]),
			exit(normal);
		{send_message_response, Response} ->
			if
				Response == 'Need to re-login' ->
					start(UserName);
				Response == 'Message sent' ->
					client(SPID, UserName);
				true ->				
					io:format("response from send ~w~n",[Response]),
					client(SPID, UserName)
			end
	end.

