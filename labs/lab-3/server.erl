-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, _ClientId, _Nick}) ->
	%NewDict = dict:append(_Nick, _ClientId, St#server_st.clients),
	case catch( dict:append(_ClientId, _Nick, St#server_st.clients) ) of
		{'EXIT',_} ->
			Return = {error, St};
		Result ->
			X = St#server_st{clients = Result},
			Return = {ok, X}
	end,
	%io:format("contents: ~p~n", [NewList]),
	Return;

loop(St, {disconnect, _Nick}) ->
	DelDict = dict:erase(_Nick, St#server_st.clients),
	X = St#server_st{clients = DelDict},
	%io:format("contents of deleted list: ~p~n", [X]),
	{ok, X};

loop(St, {join, _Nick, _Channel}) ->
	NewList = list:append(St#server_st.nick_to_channel, {_Nick, _Channel}),
	X = St#server_st{nick_to_channel = NewList},
	{ok, X};

loop(St, {msg_from_client, _From, _Channel, _Msg}) ->
	%Send message to all clients in _Channel
	Nick = dict:fetch(_From, St#server_st.clients),
	distribute(St#server_st.nick_to_channel, _Channel, Nick, _Msg),
	{ok, St};

loop(St, _Msg) -> 
    {ok, St}. 

distribute([X|XS], _Channel, _Nick, _Msg) ->
	distribute(XS, _Channel, _Nick, _Msg)
	ok;
distribute([], _Channel, _Nick, _Msg) ->
	ok.

initial_state(_Server) ->
    #server_st{name = "shire", clients = dict:new(), nick_to_channel = []}.