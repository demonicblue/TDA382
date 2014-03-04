-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").

loop(St, {connect, {_Server, _Machine}}) ->
    if St#cl_st.server /= "" ->
        {{error, user_already_connected, "User already connected"}, St};
    true ->
        trace(["Trying to connect to node: ", _Server, _Machine]),
        Result = catch_fatal(fun() -> genserver:request({list_to_atom(_Server), list_to_atom(_Machine)}, {connect, self(), St#cl_st.nick}) end),
        case Result of
        ok ->
            NewState = St#cl_st{server = _Server},
            {ok, NewState};
        error ->
            {{error, server_not_reached, "Could not reach server!"}, St};
        {error, user_already_connected} ->
            {{error, user_already_connected, "User already connected!"}, St}
        end
    end;

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    
    %If the server is already present in the client state, don't connect again.
    if St#cl_st.server /= "" ->
        {{error, user_already_connected, "User already connected"}, St};
    true ->
        case whereis(list_to_atom(_Server)) of
            %If the server atom couldn't be found, server couldn't be reached.
            undefined ->
                {{error, server_not_reached, "Server could not be reached."}, St};
            _ ->
                %Contact server to request a connection.
                Result = catch_fatal(fun() -> genserver:request(list_to_atom(_Server), {connect, self(), St#cl_st.nick}) end),
                case Result of
                     ok     ->
                        NewState = St#cl_st{server = _Server},
                        {ok, NewState};
                     {error, user_already_connected}  -> 
                        {{error, user_already_connected, "User already connected!"}, St}
                end
        end
    end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) -> 
    %If there's no server present in the client state, disconnect shouldn't work.
    if St#cl_st.server == "" ->
        {{error, user_not_connected, "User is not connected to any server!"}, St}; 
    true ->
        %If the client hasn't left the channels, return error.
        if St#cl_st.channels /= [] ->
            {{error, leave_channels_first, "Leave channels before disconnecting!"}, St};
        true ->
            %Contact server to request disconnect from server.
            Result = catch_fatal(fun() -> genserver:request(list_to_atom(St#cl_st.server), {disconnect, St#cl_st.nick}) end),
            case Result of
                ok  ->
                        NewState = St#cl_st{server = ""},
                        {ok, NewState};
                error ->
                        {{error, server_not_reached, "Could not reach the server!"}, St}
            end
        end
    end;

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
    case lists:member(_Channel, St#cl_st.channels) of
        true -> 
            {{error, user_already_joined, "User has already joined this channel!"}, St};
        false -> 
            %Contact channel process to join channel.
            genserver:request(list_to_atom(St#cl_st.server), {join, St#cl_st.nick, _Channel}),
            NewList = lists:append(St#cl_st.channels, [_Channel]),
            {ok, St#cl_st{channels = NewList}}
    end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
    case lists:member(_Channel, St#cl_st.channels) of
        true ->
            %Contact channel process to request a leave from the channel.
            genserver:request(list_to_atom(_Channel), {leave, St#cl_st.nick}),
            NewList = lists:delete(_Channel, St#cl_st.channels),
            {ok, St#cl_st{channels = NewList}};
        false ->
            %If channel is not present in the client state, leaving the channel shouldn't be possible.
            {{error, user_not_joined, "User has not joined the channel!"}, St}
    end;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
    case lists:member(_Channel, St#cl_st.channels) of
        true ->
            %Contact channel process to request sending a message.
            genserver:request(list_to_atom(_Channel), {msg_from_client, St#cl_st.nick, _Msg}),
            {ok, St} ;
        false ->
            %If user tries to send message to a channel that is not present in the client state then return error.
            {{error, user_not_joined, "Must join channel first"}, St}
    end;
     


%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
    {St#cl_st.nick, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
    NewState = St#cl_st{nick = _Nick},
    {ok, NewState} ;

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {ok, St} ;

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
    %Server already returns correct format of the message.
    _MsgFromClient.

request(St, Data) ->
    if St#cl_st.machine == "" ->
        genserver:request(St#cl_st.server, Data);
    true ->
        genserver:request( {list_to_atom(St#cl_st.server), list_to_atom(St#cl_st.machine)}, Data)
    end.

%Catch function used for eventual errors.
catch_fatal(Cmd) ->
    case catch( Cmd() ) of
        {'EXIT',Reason} ->
            trace(["EXIT:", Reason]),
            error ;
        {error, _, Msg} ->
            trace(["Error:", Msg]),
            error ;
        Result -> 
            Result
    end.

trace(Args) ->
    io:format("~n~s"++lists:flatten(lists:duplicate(length(Args)-1,"~p")),Args).


initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, server = "", machine = "", channels = [] }.
