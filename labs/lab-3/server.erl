-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, _ClientId, _Nick}) ->
	case  dict:find(_Nick, St#server_st.clients) of
		error ->	%If not found in the dict, then add client to server.
			NewDict = dict:append(_Nick, _ClientId, St#server_st.clients),
			X = St#server_st{clients = NewDict},
			{ok, X};
		_ ->
			{{error, nick_taken}, St}
	end;

loop(St, {disconnect, _Nick}) ->
	DelDict = dict:erase(_Nick, St#server_st.clients),
	X = St#server_st{clients = DelDict},
	{ok, X};

loop(St, {join, _Nick, _Channel}) ->
	case dict:is_key(_Channel, St#server_st.nick_to_channel) of
		false ->
			NewDict = dict:append(_Channel, [_Nick], St#server_st.nick_to_channel);
		true ->
			NewDict = dict:update(_Channel, fun(_List) -> lists:append(_List, [_Nick]) end, St#server_st.nick_to_channel)
	end,
	X = St#server_st{nick_to_channel = NewDict},
	%io:format(dict:fetch(_Channel, X#server_st.nick_to_channel)),
	{ok, X};

loop(St, {msg_from_client, _Nick, _Channel, _Msg}) ->
	%Send message to all clients in _Channel
	%Nick = dict:fetch(_Nick, St#server_st.clients),
	distribute(St#server_st.nick_to_channel, _Channel, _Nick, _Msg),
	{ok, St};

loop(St, {leave, _Nick, _Channel}) ->
	NewDict = dict:update(_Channel, fun(_List) -> lists:delete(_Nick, _List) end, St#server_st.nick_to_channel),
	X = St#server_st{nick_to_channel = NewDict},
	{ok, X};

loop(St, _Msg) -> 
    {ok, St}. 

distribute([X|XS], _Channel, _Nick, _Msg) ->
	distribute(XS, _Channel, _Nick, _Msg),
	ok;
distribute([], _Channel, _Nick, _Msg) ->
	ok.

initial_state(_Server) ->
    #server_st{name = "shire", clients = dict:new(), nick_to_channel = dict:new()}.