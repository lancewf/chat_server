-module(chat_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mess_interface.hrl").

get_user_name_from_pid_not_found_test() -> 
	UserMap = #{},
	UserPID = pid,
	R = chat_server:get_user_name_from_pid(UserMap, UserPID),
	R = false.

get_user_name_from_pid_found_test() -> 
	UserPID = 'hehe',
	UserMap = #{lance => #user_data{pid=UserPID, messages=[]}},
	R = chat_server:get_user_name_from_pid(UserMap, UserPID),
	R = {true, lance}.
