-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    case whereis(list_to_atom(_Server)) of
        undefined ->
            Return = {error, "Cannot connect to server"};
        _ ->
            Result = catch_fatal(fun() -> genserver:request(list_to_atom(_Server), {connect, self(), St#cl_st.nick}) end),
            Return = case Result of
                 ok     ->  trace(["Client:got ok"]),
                            {ok, St};
                 error  ->  trace(["Client:got error"]),
                            {error, "Fatal during connect"}
            end
    end,
    Return ;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) -> % Ha server som en variabel i Client-state?
    %case whereis(list_to_atom(_Server)) of
    %    undefined ->
    %        Return = {error, "Cannot disconnect from server"};
     %   _ ->
    Result = catch_fatal(fun() -> genserver:request(list_to_atom("shire"), {disconnect, self(), St#cl_st.nick}) end),
    Return = case Result of
        ok  ->  trace(["Client got ok"]),
                {ok, St};
        error -> trace(["Client got error"]),
                {error, "Fatal during disconnect"}
    end,
    Return; 

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
    {ok, St} ;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
     {ok, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
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
        {error, _, Msg} -> trace(["Error:", Msg]),
                           error ;
        Result          -> Result
    end.

trace(Args) ->
    io:format("~n~s"++lists:flatten(lists:duplicate(length(Args)-1,"~p")),Args).


initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick }.
