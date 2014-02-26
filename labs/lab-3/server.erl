-module(server).
-export([loop/2, initial_state/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("./defs.hrl").

loop(St, {connect, _ClientId, _Nick}) ->
	case  dict:find(_Nick, St#server_st.clients) of
		error ->	%If not found in the dict, then add client to server.
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
	ClientId = dict:fetch(_Nick, St#server_st.clients),
	case dict:is_key(_Channel, St#server_st.channels) of
		false ->
			ChannelId = genserver:start(list_to_atom(_Channel), channel:initial_state(_Channel, _Nick, ClientId), 
                    fun channel:loop/2),
			NewDict = dict:store(_Channel, ChannelId, St#server_st.channels),
			{ok, St#server_st{channels = NewDict}};
		true ->
			genserver:request(list_to_atom(_Channel), {join, _Nick, ClientId}),
			{ok, St}
	end;

loop(St, {msg_from_client, _FromNick, _Channel, _Msg}) ->
	%Send message to all clients in _Channel
	%Nick = dict:fetch(_Nick, St#server_st.clients),
	%distribute(St#server_st.nick_to_channel, _Channel, _Nick, _Msg),
	%?debugMsg("Server: Sending messages to clients.."),
	%io:format("~p ~p ~p", [_FromNick, _Channel, _Msg]),
	F = fun () ->
		Nicks = dict:fetch(_Channel, St#server_st.nick_to_channel),
		lists:map(fun(_ToNick) -> sendMsg(_ToNick, _FromNick, _Channel, _Msg, St) end, Nicks)
	end,
	%spawn(F),
	F(),
	{ok, St};

loop(St, {leave, _Nick, _Channel}) ->
	NewDict = dict:update(_Channel, fun(_List) -> lists:delete(_Nick, _List) end, St#server_st.nick_to_channel),
	X = St#server_st{nick_to_channel = NewDict},
	{ok, X};

loop(St, _Msg) -> 
    {ok, St}. 

sendMsg(_ToNick, _FromNick, _Channel, _Msg, St) ->
	case dict:find(_ToNick, St#server_st.clients) of
		error ->
			%io:format("~p ~p ~p", [_ToNick, " Not in dictionary", St#server_st.clients]);
			ok;
		{ok, ClientId} ->
			if _ToNick == _FromNick ->
				ok;
			true ->
				%?debugFmt("Server: Sending to ~p", [_ToNick]),
				%io:format("Sending to: ~p", [_ToNick]),
				F = fun () ->
					genserver:request(ClientId, {_Channel, _FromNick, _Msg})
				end,
				spawn(F)
				%F()
			end
	end,
	_ToNick.

initial_state(_Server) ->
    #server_st{name = "shire", clients = dict:new(), nick_to_channel = dict:new(), channels = dict:new()}.