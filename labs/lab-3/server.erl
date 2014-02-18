-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, _ClientId, _Nick}) ->
	NewList = lists:append(St#server_st.clients, [_ClientId]),
	X = St#server_st{clients = NewList},
	io:format("contents: ~p~n", [NewList]),
	{ok, X};

loop(St, {disconnect, _ClientId, _Nick}) ->
	DelList = lists:delete(_ClientId, St#server_st.clients),
	X = St#server_st{clients = DelList},
	io:format("contents of deleted list: ~p~n", [X]),
	{ok, X};

loop(St, _Msg) -> 
    {ok, St}. 

initial_state(_Server) ->
    #server_st{name = "shire", clients = []}.