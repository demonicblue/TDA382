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
                %trace(["Cannot connect to server"]),
                %io:format("~p", [St#cl_st.gui]),
                Return = {{error, server_not_reached, "Server could not be reached."}, St};
            _ ->
                Result = catch_fatal(fun() -> genserver:request(list_to_atom(_Server), {connect, self(), St#cl_st.nick}) end),
                Return = case Result of
                     ok     ->  trace(["Client:got ok"]),
                                NewState = St#cl_st{server = _Server},
                                {ok, NewState};
                     error  ->  {{error, nick_taken, "Nickname already taken!"}, St}
                end
        end;
    true ->
        Return = {{error, user_already_connected, "DU HAR JU REDAN CONNECTAT FÖR FAAAAAEN!"}, St}
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
        Result = catch_fatal(fun() -> genserver:request(list_to_atom(St#cl_st.server), {disconnect, St#cl_st.nick}) end),
        Return = case Result of
            ok  ->  trace(["Client got ok"]),
                    NewState = St#cl_st{server = ""},
                    {ok, NewState};
            error -> trace(["Client got error"]),
                    {error, St}
        end
    end,
    Return; 

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
    %Update with support for multiple channels?
    genserver:request(list_to_atom(St#cl_st.server), {join, St#cl_st.nick, _Channel}),
    NewState = St#cl_st{channels = _Channel},
    {ok, NewState} ;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
     {ok, St} ;

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
    gen_server:call(list_to_atom(St#cl_st.gui), {msg_to_GUI, St#cl_st.channels, "CONNECTION ERROR"}),
    {St#cl_st.nick, St} ;

%%%%%%%%%%
%%% Nick""
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
    X = St#cl_st{nick = _Nick},
    {ok, X} ;

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
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
    #cl_st { gui = GUIName, nick = Nick, server = "" }.
