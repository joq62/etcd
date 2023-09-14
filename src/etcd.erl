%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(etcd).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
 
%% resource_discovery 
-define(LocalResourceTuples,[{etcd,node()}]).
-define(TargetTypes,[etcd]).

%% API
-export([
	 ping/0,
	 start/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stop()-> gen_server:call(?SERVER, {stop},infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
 %% Announce to resource_discovery
    Interval=10*1000,
    Iterations=10,
    true=check_rd_running(Interval,Iterations,false),
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
      
    %% ----------------------

   %  case lists:delete(node(),sd:get_node(etcd)) of
   
  %  case lists:delete(node(),rd:fetch_resources(etcd)) of
    DbEtcdResources=[Node||Node<-nodes(),
			   pong=:=rpc:call(Node,etcd,ping,[],5000)],
    case DbEtcdResources  of
	[]->
	    ?LOG_NOTICE("First Dbase Node ",[node()]),	  
	    lib_db:dynamic_db_init([]);
	DbEtcdResources ->
	    io:format("DbEtcdResources ~p~n",[{node(),DbEtcdResources,?MODULE,?LINE}]),
	    ?LOG_NOTICE("Added Dbase Node ",[node(),DbEtcdResources]),	  
	    lib_db:dynamic_db_init(DbEtcdResources),
	    ok
    end,   
      
    ?LOG_NOTICE("Server started ",[]),
    
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};


handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.


handle_info(timeout, State) ->
    case lists:delete(node(),rd:fetch_resources(etcd)) of
	[]->
	    ?LOG_NOTICE("First Dbase Node ",[node()]),	  
	    lib_db:dynamic_db_init([]);
	DbEtcdResources ->
	    io:format("DbEtcdResources ~p~n",[{node(),DbEtcdResources,?MODULE,?LINE}]),
	    ?LOG_NOTICE("Added Dbase Node ",[node(),DbEtcdResources]),	  
	    lib_db:dynamic_db_init(DbEtcdResources),
	    ok
    end,  
    {noreply, State};

handle_info(Info, State) ->
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_rd_running(_Interval,N_,true)->
    true;
check_rd_running(Interval,0,true)->
    true;
check_rd_running(Interval,0,false)->
    false;
check_rd_running(Interval,N,IsRunning)->
    case rpc:call(node(),rd,ping,[],5000) of
	pong->
	    NewIsRunning=true,
	    NewN=N;
	_->
	    timer:sleep(Interval),
	    NewIsRunning=false,
	    NewN=N-1
    end,
    check_rd_running(Interval,NewN,NewIsRunning).
	 
