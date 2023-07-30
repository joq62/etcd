%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(etcd_provider).
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
 

%% API

-export([
	 create/1,
	 all_providers/0,
	 get_info/1,
	 
	 get_vsn/1,
	 set_vsn/2,
	 get_app/1,
	 set_app/2,

	 get_erl_args/1,
	 set_erl_args/2,
	 get_git_path/1,
	 set_git_path/2,

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
%% get all providers that are stored in dbase
%% @end
%%--------------------------------------------------------------------
-spec all_providers() -> ListOfProvidersApplName :: term().

all_providers()->
    gen_server:call(?SERVER, {all_providers},infinity).
    

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to provider ApplName 
%% @end
%%--------------------------------------------------------------------
-spec get_info(ApplName :: string()) -> {ok,ProviderInfo :: term()} | {error, Error :: term()}.

get_info(ApplName)->
    gen_server:call(?SERVER, {get_info,ApplName},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new instance with undefined data 
%% @end
%%--------------------------------------------------------------------
-spec create(ApplName :: string()) -> ok | {error, Error :: term()}.

create(ApplName)->
    gen_server:call(?SERVER, {create,ApplName},infinity).
    

%%--------------------------------------------------------------------
%% @doc
%% set vsn Vsn for provider ApplName
%% @end
%%--------------------------------------------------------------------
-spec set_vsn(Vsn :: string(), ApplName :: string()) -> ok | {error, Error :: term()}.

set_vsn(Vsn,ApplName)->
    gen_server:call(?SERVER, {set_vsn,Vsn,ApplName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get vsn for provider ApplName
%% @end
%%--------------------------------------------------------------------
-spec get_vsn(ApplName :: string()) -> {ok,Vsn :: string()} | {error, Error :: term()}.

get_vsn(ApplName)->
    gen_server:call(?SERVER, {get_vsn,ApplName},infinity).

%%--------------------------------------------------------------------
%% @doc
%% set app Appn for provider ApplName
%% @end
%%--------------------------------------------------------------------
-spec set_app(App :: atom(), ApplName :: string()) -> ok | {error, Error :: term()}.

set_app(App,ApplName)->
    gen_server:call(?SERVER, {set_app,App,ApplName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get app from provider ApplName
%% @end
%%--------------------------------------------------------------------
-spec get_app(ApplName :: string()) -> {ok,App :: atom()} | {error, Error :: term()}.

get_app(ApplName)->
    gen_server:call(?SERVER, {get_app,ApplName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% set erl_args ErlArgs for provider ApplName
%% @end
%%--------------------------------------------------------------------
-spec set_erl_args(ErlArgs :: string(), ApplName :: string()) -> ok | {error, Error :: term()}.

set_erl_args(ErlArgs,ApplName)->
    gen_server:call(?SERVER, {set_erl_args,ErlArgs,ApplName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get erl_args from provider ApplName
%% @end
%%--------------------------------------------------------------------
-spec get_erl_args(ApplName :: string()) -> {ok,ErlArgs :: string()} | {error, Error :: term()}.

get_erl_args(ApplName)->
    gen_server:call(?SERVER, {get_erl_args,ApplName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% set git_path GitPathfor  provider ApplName
%% @end
%%--------------------------------------------------------------------
-spec set_git_path(GitPath :: string(), ApplName :: string()) -> ok | {error, Error :: term()}.

set_git_path(GitPath,ApplName)->
    gen_server:call(?SERVER, {set_git_path,GitPath,ApplName},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get git  path for provider ApplName
%% @end
%%--------------------------------------------------------------------
-spec get_git_path(ApplName :: string()) -> {ok,GitPath :: string()} | {error, Error :: term()}.

get_git_path(ApplName)->
    gen_server:call(?SERVER, {get_git_path,ApplName},infinity).
    
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
    ok=lib_provider:create_table(),    
    ProviderList=lib_etcd_provider:git_clone_load(),
    Ok_ProviderList=[X||{ok,X}<-ProviderList],
    FailedToCreate=[X||{error,X}<-ProviderList],

    ?LOG_NOTICE("Successfully created  ",[Ok_ProviderList]),
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
handle_call({all_providers}, _From, State) ->
    Reply=lib_etcd_provider:get_all_id(),
    {reply, Reply, State};


handle_call({get_info,ApplName}, _From, State) ->
    Reply=lib_etcd_provider:get_info(ApplName),
    {reply, Reply, State};

handle_call({get_vsn,ApplName}, _From, State) ->
    Reply=lib_etcd_provider:get(vsn,ApplName),
    {reply, Reply, State};

handle_call({get_app,ApplName}, _From, State) ->
    Reply=lib_etcd_provider:get(app,ApplName),
    {reply, Reply, State};

handle_call({get_erl_args,ApplName}, _From, State) ->
    Reply=lib_etcd_provider:get(erl_args,ApplName),
    {reply, Reply, State};

handle_call({get_git_path,ApplName}, _From, State) ->
    Reply=lib_etcd_provider:get(git_path,ApplName),
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
