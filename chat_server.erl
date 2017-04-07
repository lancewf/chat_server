-module(chat_server).

-export([server/0, get_user_name_from_pid/2]).
-include("mess_interface.hrl").


server() ->
	process_flag(trap_exit, true),
	server(#{}).

server(Users) ->
	receive
		#send_message{message=MessageFromUser} -> 
			Message = MessageFromUser#message{timestamp = get_timestamp()},
			FromUserName = Message#message.from_username,
			ToUserName = Message#message.to_username,
			io:format("sending message from ~w to ~w ~n", [FromUserName, ToUserName]),
			ToUserData = maps:get(ToUserName, Users, none),
			FromUserData = maps:get(FromUserName, Users, none),
			if
				ToUserData == none ->
					FromUserData#user_data.pid ! {send_message_response, 'User not known in'},
					server(Users);
				ToUserData#user_data.pid == none ->
					FromUserData#user_data.pid ! {send_message_response, 'User is offline'},
					server(update_user_data_for_offline_message(Users, Message));
				true ->
					ToUserData#user_data.pid ! #receive_message{message=Message},
					FromUserData#user_data.pid ! {send_message_response, 'Message sent'},
					server(Users)
			end;
		#login_user{username=UserName, pid=PID} -> 
			Valid = is_valid_login(UserName),
			if
				Valid ->
					io:format("adding user ~w ~n", [UserName]),
					PID ! logged_in,
					link(PID),
					OfflineMessages = get_offline_messages(Users, UserName),
					send_offline_messages(OfflineMessages, PID),
					server(update_user_data_for_login(Users, UserName, PID));
				true ->
					PID ! {bad_log_in, 'Not allowed access'},
					server(Users)
			end;
		#remove_user{username=UserName} -> 
			io:format("removed user ~w ~n", [UserName]),
			server(update_user_data_for_logout(Users, UserName));
		print_users ->
			io:format("Users = ~w ~n", [Users]),
			server(Users);
                {'EXIT', FromPID, _} ->
			case get_user_name_from_pid(Users, FromPID) of
				false -> 
					server(Users);
				{true, UserName} -> 
					server(update_user_data_for_logout(Users, UserName))
			end;
		finish ->
			io:format("sender finish Users = ~w ~n", [Users]),
			exit(normal)
	end.

update_user_data_for_offline_message(UserMap, Message) ->
	ToUserName = Message#message.to_username,
	ExistingUser = maps:is_key(ToUserName, UserMap),
	if
		ExistingUser ->
			UserData = maps:get(ToUserName, UserMap),
			UpdatedMessages = lists:append(UserData#user_data.messages, [Message]),
			UserMap#{ToUserName => UserData#user_data{messages=UpdatedMessages}};
		true ->
			UserMap
	end.

get_timestamp() ->
	{Mega, Seconds, _} = erlang:timestamp(),
	Mega * 1000000 + Seconds.

get_user_name_from_pid(UserMap, UserPID) when is_map(UserMap) ->
	Users = maps:to_list(UserMap),
	case lists:dropwhile(fun ({_, #user_data{pid=PID}}) -> PID =/= UserPID end, Users) of
		[{UserName, _}|_] -> {true, UserName};
		[] -> false
	end.

update_user_data_for_logout(UserMap, UserName) ->
	ExistingUser = maps:is_key(UserName, UserMap),
	if
		ExistingUser ->
			UserData = maps:get(UserName, UserMap),
			UserMap#{UserName => UserData#user_data{pid=none}};
		true ->
			UserMap
	end.

get_offline_messages(Users, UserName) ->
	UserData = maps:get(UserName, Users, none),
	if
		UserData =:= none ->
			[];
		true ->
			UserData#user_data.messages
	end.

send_offline_messages([], _) ->
	ok;
send_offline_messages([Message|Rest], ToUserPID) ->
	ToUserPID ! #receive_message{message=Message},
	send_offline_messages(Rest, ToUserPID).

update_user_data_for_login(UserMap, UserName, UserPID) ->
	ExistingUser = maps:is_key(UserName, UserMap),
	if
		ExistingUser ->
			UserData = maps:get(UserName, UserMap),
			UserMap#{UserName => UserData#user_data{pid=UserPID, messages=[]}};
		true ->
			UserMap#{UserName => #user_data{pid=UserPID, messages=[]}}
	end.

is_valid_login(UserName) ->
	if
		UserName == bad_guy ->
			false;
		true ->
			true
	end.
