%% THIS module is not compiled directly, but it used to build a custom sever

-module(custom_server).

-behaviour(gen_server).

%% API
-export([start_link/0, make_supervisor_childspec/0,db_state/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([]).

-define(SERVER, ?MODULE). 



-spec(make_supervisor_childspec() ->
	     {ok, supervisor:childspec()}).
make_supervisor_childspec() ->
    {ok, {custom_server,
          {custom_server, start_link, []},
          permanent,100,worker,[custom_server]}}.


interface() ->
    ok.

-spec(start_link() ->
	      {ok,pid()}).
start_link() ->
    gen_server:start_link( ?MODULE, [], []).

init(_) ->
    {ok,DBState} = db_state(),
    Erlog        = erlog:new(DBState),
    {ok,  Erlog}.

handle_prolog(Request, Erlog, last) ->
    case Erlog({prove, Request}) of
	{{succeed, [{'Return', Return}]}, E1} ->
	    {reply, Return, E1};
	{fail, E1} ->
	    {reply, fail, E1}
    end;
handle_prolog(Request, Erlog, none) ->
    case Erlog({prove, Request}) of
	{{succeed, _}, E1} ->
	    {reply, true, E1};
	{fail, E1} ->
	    {reply, false, E1}
    end.

handle_call(Request = {add_sib,_A,_B}, _From,  Erlog) ->
    handle_prolog(Request, Erlog, none);
handle_call(Request,_, Erlog) ->
    io:format("Error unknown request ~p~n", [Request]),
    {reply, error, Erlog}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->  
    {noreply, State}.

terminate(_Reason, _State) ->    
    ok.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.

-spec(db_state() -> {ok, erlog:db_state()}).
db_state() ->
    {ok,[]}.
