-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, _ClientId, _Nick}) ->
	NewDict = dict:append(_Nick, _ClientId, St#server_st.clients),
	X = St#server_st{clients = NewDict},
	%io:format("contents: ~p~n", [NewList]),
	{ok, X};

loop(St, {disconnect, _Nick}) ->
	DelDict = dict:erase(_Nick, St#server_st.clients),
	X = St#server_st{clients = DelDict},
	%io:format("contents of deleted list: ~p~n", [X]),
	{ok, X};

loop(St, _Msg) -> 
    {ok, St}. 

initial_state(_Server) ->
    #server_st{name = "shire", clients = dict:new(), nick_to_channel = []}.