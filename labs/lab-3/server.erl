-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, _ClientId, _Nick}) ->
	case  dict:find(_Nick, St#server_st.clients) of
		error ->
			NewDict = dict:store(_Nick, _ClientId, St#server_st.clients),
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
			NewDict = dict:store(_Channel, [_Nick], St#server_st.nick_to_channel);
		true ->
			NewDict = dict:update(_Channel, fun(_List) -> lists:append(_List, [_Nick]) end, St#server_st.nick_to_channel)
	end,
	X = St#server_st{nick_to_channel = NewDict},
	%io:format(dict:fetch(_Channel, X#server_st.nick_to_channel)),
	io:format("~p", [dict:fetch(_Nick, X#server_st.clients)]),
	{ok, X};

loop(St, {msg_from_client, _FromNick, _Channel, _Msg}) ->
	%Send message to all clients in _Channel
	%Nick = dict:fetch(_Nick, St#server_st.clients),
	%distribute(St#server_st.nick_to_channel, _Channel, _Nick, _Msg),
	io:format("~p ~p ~p", [_FromNick, _Channel, _Msg]),
	Nicks = dict:fetch(_Channel, St#server_st.nick_to_channel),
	lists:map(fun(_ToNick) -> sendMsg(_ToNick, _FromNick, _Channel, _Msg, St) end, Nicks),
	{ok, St};

loop(St, _Msg) -> 
    {ok, St}. 

sendMsg(_ToNick, _FromNick, _Channel, _Msg, St) ->
	case dict:find(_ToNick, St#server_st.clients) of
		error ->
			io:format("~p ~p ~p", [_ToNick, " Not in dictionary", St#server_st.clients]);
		{ok, ClientId} ->
			if _ToNick == _FromNick ->
				ok;
			true ->
				io:format("Sending to: ~p", [_ToNick]),
				genserver:request(ClientId, {_Channel, _FromNick, _Msg})
			end
	end,
	_ToNick.

distribute([X|XS], _Channel, _Nick, _Msg) ->
	distribute(XS, _Channel, _Nick, _Msg),
	ok;
distribute([], _Channel, _Nick, _Msg) ->
	ok.

initial_state(_Server) ->
    #server_st{name = "shire", clients = dict:new(), nick_to_channel = dict:new()}.