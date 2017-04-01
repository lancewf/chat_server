-module(chat_server).

-export([server/0]).
-include("mess_interface.hrl").


server() ->
	process_flag(trap_exit, true),
	server(#{}).

server(Users) ->
	receive
		#send_message{from_username=FromUserName, to_username=ToUserName, message=Message} -> 
			io:format("sending message from ~w to ~w ~n", [FromUserName, ToUserName]),
			ToPID = maps:get(ToUserName, Users, none),
			FromPID = maps:get(FromUserName, Users, none),
			if
				ToPID == none ->
					FromPID ! {send_message_response, 'User not logged in'};
				true ->
					ToPID ! {recive_message, FromUserName, Message},
					FromPID ! {send_message_response, 'Message sent'}
			end,
			server(Users);
		#login_user{username=UserName, pid=PID} -> 
			Valid = is_valid_login(UserName),
			if
				Valid ->
					io:format("adding user ~w ~n", [UserName]),
					PID ! logged_in,
					link(PID),
					server(Users#{UserName => PID});
				true ->
					PID ! {bad_log_in, 'Not allowed access'},
					server(Users)
			end;
		#remove_user{username=UserName} -> 
			io:format("removed user ~w ~n", [UserName]),
			server(maps:remove(UserName, Users));
		print_users ->
			io:format("Users = ~w ~n", [Users]),
			server(Users);
                {'EXIT', From, _} ->
			%io:format("Exit From ~w What? ~w ~n", [From, What]),
			server(removeUserFromPID(maps:to_list(Users), From, #{}));
		finish ->
			io:format("sender finish Users = ~w ~n", [Users]),
			exit(normal)
	end.

is_valid_login(UserName) ->
	if
		UserName == bad_guy ->
			false;
		true ->
			true
	end.

removeUserFromPID([], _, NewUsersMap) ->
	NewUsersMap;

removeUserFromPID([{UserName, ClientPID}|RestOfUsers], PIDToRemove, NewUsersMap) when ClientPID == PIDToRemove ->
	io:format("removing user: ~w ~n", [UserName]),
	removeUserFromPID(RestOfUsers, PIDToRemove, NewUsersMap);
	
removeUserFromPID([{UserName, ClientPID}|RestOfUsers], PIDToRemove, NewUsersMap) ->
	removeUserFromPID(RestOfUsers, PIDToRemove, NewUsersMap#{UserName => ClientPID}).

