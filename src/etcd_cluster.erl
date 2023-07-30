%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(etcd_cluster).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
 

%% API

-export([
	 create/1,
	 all_clusters/0,
	 get_info/1,
	 
	 get_cookie_str/1,
	 get_deployment_spec/1,
	 get_deployment_records/1,
	 set_deployment_records/2,	 
	 
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
%% get all clusternames that are stored in dbase
%% @end
%%--------------------------------------------------------------------
-spec all_clusters() -> ListOfClusternames :: term().

all_clusters()->
    gen_server:call(?SERVER, {all_clusters},infinity).
    

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_info(ClusterName :: string()) -> {ok,ClusterInfo :: term()} | {error, Error :: term()}.

get_info(ClusterName)->
    gen_server:call(?SERVER, {get_info,ClusterName},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new instance with undefined data 
%% @end
%%--------------------------------------------------------------------
-spec create(ClusterName :: string()) -> ok | {error, Error :: term()}.

create(ClusterName)->
    gen_server:call(?SERVER, {create,ClusterName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get cookie_str for cluster ClusterName 
%% @end
%%--------------------------------------------------------------------
-spec get_cookie_str(ClusterName :: string()) -> {ok,CookieStr :: string()} | {error, Error :: term()}.

get_cookie_str(ClusterName)->
    gen_server:call(?SERVER, {get_cookie_str,ClusterName},infinity).

    
%%--------------------------------------------------------------------
%% @doc
%% get deployment_spec for cluster ClusterName 
%% @end
%%--------------------------------------------------------------------
-spec get_deployment_spec(ClusterName :: string()) -> {ok,DeploymentSpec :: string()} | {error, Error :: term()}.

get_deployment_spec(ClusterName)->
    gen_server:call(?SERVER, {get_deployment_spec,ClusterName},infinity).
        
%%--------------------------------------------------------------------
%% @doc
%% set deployment_records for cluster ClusterName 
%% @end
%%--------------------------------------------------------------------
-spec set_deployment_records(ClusterName :: string(),DeploymentRecords :: term()) -> ok | {error, Error :: term()}.

set_deployment_records(ClusterName,DeploymentRecords)->
    gen_server:call(?SERVER, {set_deployment_records,ClusterName,DeploymentRecords},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get deployment_records for cluster ClusterName 
%% @end
%%--------------------------------------------------------------------
-spec get_deployment_records(ClusterName :: string()) -> {ok,DeploymentRecords :: term()} | {error, Error :: term()}.

get_deployment_records(ClusterName)->
    gen_server:call(?SERVER, {get_deployment_records,ClusterName},infinity).
    
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
    ok=lib_etcd_cluster:create_table(),    
    ClusterSpecList=lib_etcd_cluster:git_clone_load(),
    Ok_ClusterSpec=[X||{ok,X}<-ClusterSpecList],
    FailedToCreate=[X||{error,X}<-ClusterSpecList],

    ?LOG_NOTICE("Successfully created  ",[Ok_ClusterSpec]),
    case FailedToCreate of
	[]->
	    ok;
	_->
	    ?LOG_NOTICE("Failed to create   ",[FailedToCreate])
    end,
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({all_clusters}, _From, State) ->
    Reply=lib_etcd_cluster:get_all_id(),
    {reply, Reply, State};

handle_call({get_info,ClusterName}, _From, State) ->
    Reply=lib_etcd_cluster:get_info(ClusterName),
    {reply, Reply, State};

handle_call({get_cookie_str,ClusterName}, _From, State) ->
    Reply=lib_etcd_cluster:get(cookie_str,ClusterName),
    {reply, Reply, State};

handle_call({get_deployment_spec,ClusterName}, _From, State) ->
    Reply=lib_etcd_cluster:get(deployment_spec,ClusterName),
    {reply, Reply, State};

handle_call({set_deployment_records,ClusterName,DeploymentRecords}, _From, State) ->
    Reply=lib_etcd_cluster:set_deployment_records(DeploymentRecords,ClusterName),
    {reply, Reply, State};

handle_call({get_deployment_records,ClusterName}, _From, State) ->
    Reply=lib_etcd_cluster:get(deployment_records,ClusterName),
    {reply, Reply, State};

handle_call({create,ClusterName}, _From, State) ->
    Reply=lib_etcd_cluster:create(ClusterName),
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
