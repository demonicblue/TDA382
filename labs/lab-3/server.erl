-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, _ClientId, _Nick}) ->
	NewList = lists:append(St#server_st.clients, _ClientId),
	X = St#server_st{clients = NewList},
	{ok, X};

loop(St, _Msg) -> 
    {ok, St}. 


initial_state(_Server) ->
    #server_st{name = "shire", clients = []}.
