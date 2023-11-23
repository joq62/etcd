%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(etcd_infra).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
 

%% API

-export([
	 all_infra/0,
	 get_info/1,

	 get_connect_nodes/1,
	 get_cookie_str/1,
	 get_num_workers/1,
	 get_num_workers/2,
	 
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
%% get all hostnames that are stored in dbase
%% @end
%%--------------------------------------------------------------------
-spec all_infra() -> ListOfHostnames :: term().

all_infra()->
    gen_server:call(?SERVER, {all_infra},infinity).
    

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_info(InfraSpec :: string()) -> {ok,InfraInfo :: term()} | {error, Error :: term()}.

get_info(InfraSpec)->
    gen_server:call(?SERVER, {get_info,InfraSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_connect_nodes(InfraSpec :: string()) -> {ok,ConnectNodes :: term()} | {error, Error :: term()}.

get_connect_nodes(InfraSpec)->
    gen_server:call(?SERVER, {get_connect_nodes,InfraSpec},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_cookie_str(InfraSpec :: string()) -> {ok,CookieStr :: string()} | {error, Error :: term()}.

get_cookie_str(InfraSpec)->
    gen_server:call(?SERVER, {get_cookie_str,InfraSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get ip adress from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_num_workers(InfraSpec :: string()) -> {ok,NumWorkesList :: term()} | {error, Error :: term()}.

get_num_workers(InfraSpec)->
    gen_server:call(?SERVER, {get_num_workers,InfraSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get ip adress from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_num_workers(InfraSpec :: string(),HostName :: string()) -> {ok,NumWorkers :: integer()} | {error, Error :: term()}.

get_num_workers(InfraSpec,HostName)->
    gen_server:call(?SERVER, {get_num_workers,InfraSpec,HostName},infinity).


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
    case lists:delete({etcd,node()},rd:fetch_resources(etcd)) of
	[]->
	    ok=lib_etcd_infra:create_table(),    
	    SpecList=lib_etcd_infra:git_clone_load(),
	    Ok_Specs=[X||{ok,X}<-SpecList],
	    FailedToCreate=[X||{error,X}<-SpecList],
	    ?LOG_NOTICE("Successfully created  ",[Ok_Specs]),
	    case FailedToCreate of
		[]->
		    ok;
		_->
		    ?LOG_NOTICE("Failed to create   ",[FailedToCreate])
	    end;
	_ ->
	    ok
    end,
    
 
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({all_infra}, _From, State) ->
    Reply=lib_etcd_infra:get_all_id(),
    {reply, Reply, State};


handle_call({get_info,InfraSpec}, _From, State) ->
    Reply=lib_etcd_infra:get_info(InfraSpec),
    {reply, Reply, State};

handle_call({get_connect_nodes,InfraSpec}, _From, State) ->
    Reply=lib_etcd_infra:get(connect_nodes,InfraSpec),
    {reply, Reply, State};

handle_call({get_cookie_str,InfraSpec}, _From, State) ->
    Reply=lib_etcd_infra:get(cookie_str,InfraSpec),
    {reply, Reply, State};

handle_call({get_num_workers,InfraSpec}, _From, State) ->
    Reply=lib_etcd_infra:get(num_workers,InfraSpec),
    {reply, Reply, State};

handle_call({get_num_workers,InfraSpec,HostName}, _From, State) ->
    Reply=lib_etcd_infra:get(num_workers,InfraSpec,HostName),
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
