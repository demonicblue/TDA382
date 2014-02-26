-module(channel).
-export([loop/2, initial_state/3]).
-include_lib("./defs.hrl").

loop(St, {msg_from_client, _FromNick, _Msg}) ->
	F = fun () ->
		Channel = St#channel_st.name,
		dict:map(fun(_ToNick, _ClientId) ->  send_msg(_FromNick, _ToNick, _ClientId, Channel, _Msg) end, St#channel_st.clients)
	end,
	spawn(F),
	%F(),
	{ok, St};

loop(St, {join, _Nick, _ClientId}) ->
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
	if _ToNick == _FromNick ->
		ok;
	true ->
		genserver:request(_ClientId, {_Channel, _FromNick, _Msg})
	end,
	ok.

initial_state(_Name, _Nick, _ClientId) ->
	NewDict = dict:new(),
    #channel_st{name = _Name, clients = dict:store(_Nick, _ClientId, NewDict)}.