-module(channel).
-export([loop/2, initial_state/3]).
-include_lib("./defs.hrl").

loop(St, {msg_from_client, _FromNick, _Msg}) ->
	Channel = St#channel_st.name,
	%Use map-function to send message to all clients in the channel.
	dict:map(fun(_ToNick, _ClientId) ->  send_msg(_FromNick, _ToNick, _ClientId, Channel, _Msg) end, St#channel_st.clients),
	{ok, St};

loop(St, {join, _Nick, _ClientId}) ->
	%If not found in the dict, then add client to server.
	case dict:find(_Nick, St#channel_st.clients) of
		error ->
			NewDict = dict:store(_Nick, _ClientId, St#channel_st.clients),
			{ok, St#channel_st{clients = NewDict}};
		{ok, _} ->
			{error, St}
	end;

loop(St, {leave, _Nick}) ->
	NewDict = dict:erase(_Nick, St#channel_st.clients),
	{ok, St#channel_st{clients = NewDict}}.

send_msg(_FromNick, _ToNick, _ClientId, _Channel, _Msg) ->
	%Used for not sending message to oneself.
	if _ToNick == _FromNick ->
		ok;
	true ->
		F = fun () ->
			request(_ClientId, {_Channel, _FromNick, _Msg})
		end,
		% Spawn a new process for each request	
		spawn(F)
	end,
	ok.

request({Client, Machine}, Data) ->
	genserver:request({Client, Machine}, Data);

request(Client, Data) ->
	genserver:request(Client, Data).

initial_state(_Name, _Nick, _ClientId) ->
	%Add the first client that wanted to join the channel.
	NewDict = dict:new(),
    #channel_st{name = _Name, clients = dict:store(_Nick, _ClientId, NewDict)}.