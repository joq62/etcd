%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(etcd_deployment_record).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
-include("etcd_cluster.hrl").
 

%% API

-export([
	 create_records/1,
	 get_node_name/1,
	 get_node/1,
	 get_app/1,
	 get_dir/1,
	 get_provider/1,
	 get_host/1,

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
%% Creates  to take lead by locking 
%% @end
%%--------------------------------------------------------------------
-spec create_records(ClusterSpec :: string()) -> {ok,DeploymentRecords :: term()} | {error, Error :: term()}.

create_records(ClusterSpec)->
    gen_server:call(?SERVER, {create_records,ClusterSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Creates  to take lead by locking 
%% @end
%%--------------------------------------------------------------------
-spec get_node_name(Record :: term()) -> {ok,NodeName :: string()} | {error, Error :: term()}.

get_node_name(Record)->
    gen_server:call(?SERVER, {get_node_name,Record},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Creates  to take lead by locking 
%% @end
%%--------------------------------------------------------------------
-spec get_node(Record :: term()) -> {ok, Node :: node()} | {error, Error :: term()}.

get_node(Record)->
    gen_server:call(?SERVER, {get_node,Record},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Creates  to take lead by locking 
%% @end
%%--------------------------------------------------------------------
-spec get_app(Record :: term()) -> {ok,App :: atom()} | {error, Error :: term()}.

get_app(Record)->
    gen_server:call(?SERVER, {get_app,Record},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Creates  to take lead by locking 
%% @end
%%--------------------------------------------------------------------
-spec get_dir(Record :: term()) -> {ok,Dir :: string()} | {error, Error :: term()}.

get_dir(Record)->
    gen_server:call(?SERVER, {get_dir,Record},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Creates  to take lead by locking 
%% @end
%%--------------------------------------------------------------------
-spec get_provider(Record :: term()) -> {ok, Provider :: string()} | {error, Error :: term()}.

get_provider(Record)->
    gen_server:call(?SERVER, {get_provider,Record},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Creates  to take lead by locking 
%% @end
%%--------------------------------------------------------------------
-spec get_host(Record :: term()) -> {ok,Host :: string()} | {error, Error :: term()}.

get_host(Record)->
    gen_server:call(?SERVER, {get_host,Record},infinity).

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
    
 
    ?LOG_NOTICE("Server started  ",[]),
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
handle_call({get_node_name,Record}, _From, State) ->
    Reply={ok,Record#deployment_record.node_name},
    {reply, Reply, State};
handle_call({get_node,Record}, _From, State) ->
    Reply={ok,Record#deployment_record.node},
    {reply, Reply, State};
handle_call({get_app,Record}, _From, State) ->
    Reply={ok,Record#deployment_record.app},
    {reply, Reply, State};
handle_call({get_dir,Record}, _From, State) ->
    Reply={ok,Record#deployment_record.dir},
    {reply, Reply, State};
handle_call({get_provider,Record}, _From, State) ->
    Reply={ok,Record#deployment_record.provider},
    {reply, Reply, State};
handle_call({get_host,Record}, _From, State) ->
    Reply={ok,Record#deployment_record.host},
    {reply, Reply, State};

handle_call({create_records,ClusterSpec}, _From, State) ->
    {ok,CookieStr}=etcd_cluster:get_cookie_str(ClusterSpec),
 %   {ok,CookieStr}=lib_etcd_cluster:get(cookie_str,ClusterName),
    {ok,DeploymentSpec}=etcd_cluster:get_deployment_spec(ClusterSpec),
 %   {ok,DeploymentSpec}=lib_etcd_cluster:get(deployment_spec,ClusterName),
    {ok,DeploymentList}=etcd_deployment:get_deployment_list(DeploymentSpec),
    SortedDeploymentLis=lists:sort(DeploymentList),
    Num=length(SortedDeploymentLis),
    DeploymentRecords=create_records(SortedDeploymentLis,Num,CookieStr,[]),
   % io:format("DeploymentRecords ~p~n",[{DeploymentRecords,?MODULE,?LINE}]),
    Reply=case etcd_cluster:set_deployment_records(DeploymentRecords,ClusterSpec) of
	      ok->
		  {ok,DeploymentRecords};
	      Error->
		  Error
	  end,
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

create_records([],_Num,_CookieStr,Acc)->
    Acc;
create_records([{Provider,Host}|T],N,CookieStr,Acc) ->
    {ok,App}=etcd_application:get_app(Provider),
    NStr=integer_to_list(N),
    NodeName=CookieStr++"_"++NStr,
    Node=list_to_atom(NodeName++"@"++Host),
    Dir=NodeName++".provider_dir",
    R=#deployment_record{node_name=NodeName,
                         node=Node,
			 dir=Dir,
			 provider=Provider,
			 app=App,
			 host=Host},
    create_records(T,N-1,CookieStr,[R|Acc]).
    
