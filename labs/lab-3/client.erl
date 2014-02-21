-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    
    if St#cl_st.server == "" ->

        case whereis(list_to_atom(_Server)) of
            undefined ->
                Return = {{error, server_not_reached, "Server could not be reached."}, St};
            _ ->
                Result = catch_fatal(fun() -> genserver:request(list_to_atom(_Server), {connect, self(), St#cl_st.nick}) end),
                Return = case Result of
                     ok     ->  trace(["Client:got ok"]),
                                NewState = St#cl_st{server = _Server},
                                {ok, NewState};
                     error  ->  {{error, user_already_connected, "User already connected!"}, St}
                end
        end;
    true ->
        Return = {{error, user_already_connected, "User already connected"}, St}
    end,
    Return ;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) -> 
    case St#cl_st.server of
        "" ->
            Return = {{error, user_not_connected, "User is not connected to any server!"}, St};
            
        _ ->
            if St#cl_st.channels == [] -> 
                Result = catch_fatal(fun() -> genserver:request(list_to_atom(St#cl_st.server), {disconnect, St#cl_st.nick}) end),
                Return = case Result of
                    ok  ->  trace(["Client got ok"]),
                            NewState = St#cl_st{server = ""},
                            {ok, NewState};
                    error -> trace(["Client got error"]),
                            {{error, server_not_reached, "Could not reach the server!"}, St}
                end;
            true ->
                Return = {{error, leave_channels_first, "Leave channels before disconnecting!"}, St}
            end
    end,
    Return; 

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
    Equals = fun(X) -> if X == _Channel -> true; true -> false end end,
    case lists:any(Equals, St#cl_st.channels) of
        true -> 
            Return = {{error, user_already_joined, "User has already joined this channel!"}, St};
        false -> 
            genserver:request(list_to_atom(St#cl_st.server), {join, St#cl_st.nick, _Channel}),
            NewList = lists:append(St#cl_st.channels, [_Channel]),
            NewState = St#cl_st{channels = NewList},
            Return = {ok, NewState}
    end,
    Return;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
    Equals = fun(X) -> if X == _Channel -> true; true -> false end end,
    case lists:any(Equals, St#cl_st.channels) of
        true ->
            genserver:request(list_to_atom(St#cl_st.server), {leave, St#cl_st.nick, _Channel}),
            NewList = lists:delete(_Channel, St#cl_st.channels),
            NewState = St#cl_st{channels = NewList},
            Return = {ok, NewState};
        false ->
            Return = {{error, user_not_joined, "User has not joined the channel!"}, St}
    end,

    Return;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
    genserver:request(list_to_atom(St#cl_st.server), {msg_from_client, St#cl_st.nick, _Channel, _Msg}),
     {ok, St} ;


%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
    {St#cl_st.nick, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
    X = St#cl_st{nick = _Nick},
    {ok, X} ;

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    io:format("Contents of list: ~p\n", [St#cl_st.channels]),
    {St, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #cl_st { gui = GUIName }, _MsgFromClient) ->
    {Channel, Name, Msg} = decompose_msg(_MsgFromClient),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.


% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg(_MsgFromClient) ->
    {"", "", ""}.

catch_fatal(Cmd) ->
    case catch( Cmd() ) of
        {'EXIT',Reason} ->
            trace(["EXIT:", Reason]),
            error ;
        {error, nick_taken} -> 
            trace(["Mammas bröd"]),
            error;


        {error, _, Msg} ->
            trace(["Error:", Msg]),
            error ;
        Result -> 
            Result
    end.

trace(Args) ->
    io:format("~n~s"++lists:flatten(lists:duplicate(length(Args)-1,"~p")),Args).


initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, server = "", channels = [] }.
