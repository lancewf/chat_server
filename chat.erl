-module(chat).

-export([login/1, logoff/0, send/2]).
-include("mess_interface.hrl").

login([UserName|_]) ->
	login(UserName);
login(UserName) ->
	case whereis(mess_client) of
		undefined ->
			register(mess_client, spawn(chat_client, start, [UserName]));
		_ -> 
			already_logged_on
	end.

logoff() ->
	case whereis(mess_client) of
		undefined ->
			already_logged_off;
		_ -> 
			mess_client ! logoff
	end.

send(ToUserName, Body) ->
	mess_client ! #send_message{message=#message{to_username=ToUserName, body=Body}},
	ok.


