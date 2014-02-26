-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

loop(St, {connect, _ClientId, _Nick}) ->
	case  dict:find(_Nick, St#server_st.clients) of
		%If not found in the dictionary error is returned, then add client to server.
		error ->	
			NewDict = dict:store(_Nick, _ClientId, St#server_st.clients),
			X = St#server_st{clients = NewDict},
			{ok, X};
		_ ->
			{{error, user_already_connected}, St}
	end;

loop(St, {disconnect, _Nick}) ->
	DelDict = dict:erase(_Nick, St#server_st.clients),
	X = St#server_st{clients = DelDict},
	{ok, X};

loop(St, {join, _Nick, _Channel}) ->
	ClientId = dict:fetch(_Nick, St#server_st.clients),
	case dict:is_key(_Channel, St#server_st.channels) of
		%If there is no channel in the dictionary, then start new process and add this new channel to dictionary.
		false ->
			ChannelId = genserver:start(list_to_atom(_Channel), channel:initial_state(_Channel, _Nick, ClientId), 
                    fun channel:loop/2),
			NewDict = dict:store(_Channel, ChannelId, St#server_st.channels),
			{ok, St#server_st{channels = NewDict}};
		%Communicate with channel to add new user.
		true ->
			genserver:request(list_to_atom(_Channel), {join, _Nick, ClientId}),
			{ok, St}
	end;

%Catch all other, so that the server doesn't crash.
loop(St, _Msg) -> 
    {ok, St}.

initial_state(_Server) ->
    #server_st{name = "shire", clients = dict:new(), channels = dict:new()}.