%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(etcd_cluster_to_deploy).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
-include("etcd_cluster_to_deploy.hrl").
 

%% API

-export([
	 create/2,
	 get_info/0,
	 get_cluster_spec/0,
	 get_creator/0,
	 
	 ping/0,
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
%% Creates a new instance 
%% @end
%%--------------------------------------------------------------------
-spec create(ClusterSpec :: string(),Creator :: node()) -> ok | {error, Error :: term()}.
create(ClusterSpec,Creator)->
    gen_server:call(?SERVER, {create,ClusterSpec,Creator},infinity).
    
    

%%--------------------------------------------------------------------
%% @doc
%% Get all info for the deployment 
%% @end
%%--------------------------------------------------------------------
-spec get_info() -> {ok,DeploymentInfo :: term()} | {error, Error :: term()}.

get_info()->
    gen_server:call(?SERVER, {get_info},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get cluster spec that shall be deployd
%% @end
%%--------------------------------------------------------------------
-spec get_cluster_spec() -> {ok,Cluster_spec :: string()} | {error, Error :: term()}.

get_cluster_spec()->
    gen_server:call(?SERVER, {get_cluster_spec},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Get the creator node  
%% @end
%%--------------------------------------------------------------------
-spec get_creator() -> {ok,Creator :: node()} | {error, Error :: term()}.

get_creator()->
    gen_server:call(?SERVER, {get_creator},infinity).


%%--------------------------------------------------------------------
%% @doc
%%  
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
    
    ok=lib_etcd_cluster_to_deploy:create_table(),
 
    ?LOG_NOTICE("Server started  ",[]),
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------


handle_call({get_info}, _From, State) ->
    Reply=lib_etcd_cluster_to_deploy:get_info(),
    {reply, Reply, State};


handle_call({get_cluster_spec}, _From, State) ->
    Reply=lib_etcd_cluster_to_deploy:get(cluster_spec),
    {reply, Reply, State};

handle_call({get_creator}, _From, State) ->
    Reply=lib_etcd_cluster_to_deploy:get(creator),
    {reply, Reply, State};

handle_call({create,ClusterSpec,Creator}, _From, State) ->
    Reply=lib_etcd_cluster_to_deploy:create(ClusterSpec,Creator),
    {reply, Reply, State};


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
