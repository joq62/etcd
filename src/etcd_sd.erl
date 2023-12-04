%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(etcd_sd).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
-include("etcd_sd.hrl"). 

%% API

-export([
	 create/3,
	 all_services/0,
	 get_info/2,
	 
	 register/3,
	 unregister/2,
	 discover/1,
	 clean_up/0,
	 
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
-spec create(ServiceId :: atom() ,Module :: atom ,Node :: node()) -> ok | {error, Error :: term()}.

create(ServiceId,Module,Node)->
    gen_server:call(?SERVER, {create,ServiceId,Module,Node},infinity).
%%------------------------------------------------------------
%% @doc
%% get all locks name that are stored in dbase
%% @end
%%--------------------------------------------------------------------
-spec all_services() -> ListOfServices:: term().

all_services()->
    gen_server:call(?SERVER, {all_services},infinity).
    

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to lock Lock 
%% @end
%%--------------------------------------------------------------------
-spec get_info(ServiceId :: atom(),Node :: node()) -> {ok,ServiceInfo :: term()} | {error, Error :: term()}.

get_info(ServiceId,Node)->
    gen_server:call(?SERVER, {get_info,ServiceId,Node},infinity).


%%--------------------------------------------------------------------
%% @doc
%% register and reregister a service 
%% @end
%%--------------------------------------------------------------------
-spec register(ServiceId :: atom() ,Module :: atom ,Node :: node()) -> {ok,Transaction :: integer()} | 
	  {error, Error :: term()}.

register(ServiceId,Module,Node)->
    gen_server:call(?SERVER, {register,ServiceId,Module,Node},infinity).

%%--------------------------------------------------------------------
%% @doc
%% unregister a service 
%% @end
%%--------------------------------------------------------------------
-spec unregister(ServiceId :: atom(),Node :: node()) -> {ok,Transaction :: integer()} | 
	  {error, Error :: term()}.

unregister(ServiceId,Node)->
    gen_server:call(?SERVER, {unregister,ServiceId,Node},infinity).

%%--------------------------------------------------------------------
%% @doc
%% unlock 
%% @end
%%--------------------------------------------------------------------
-spec discover(ServiceId :: atom()) -> ListOfNodesModules :: term() | 
	  {error, Error :: term()}.

discover(ServiceId)->
    gen_server:call(?SERVER, {discover,ServiceId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Removes passive applications 
%% @end
%%--------------------------------------------------------------------
-spec clean_up() -> ok | 
	  {error, Error :: term()}.

clean_up()->
    gen_server:call(?SERVER, {clean_up},infinity).

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
    
  %  case lists:delete({etcd,node()},rd:fetch_resources(etcd)) of
%	[]->
%	    ok=lib_etcd_lock:create_table();
%	_ ->
%	    ok
 %   end,
%    spawn(fun()->do_clean_up() end),
    timer:send_after(?RefreshTime,clean_up),
    ?LOG_NOTICE("Server started  ",[]),
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
handle_call({all_services}, _From, State) ->
    Reply=lib_etcd_sd:get_all_id(),
    {reply, Reply, State};

handle_call({get_info,ServiceId,Node}, _From, State) ->
    Reply=lib_etcd_sd:get_info(ServiceId,Node),
    {reply, Reply, State};

handle_call({register,ServiceId,Module,Node}, _From, State) ->
    Reply=lib_etcd_sd:register(ServiceId,Module,Node),
    {reply, Reply, State};

handle_call({unregister,ServiceId,Node}, _From, State) ->
    Reply=lib_etcd_sd:unregister(ServiceId,Node),
    {reply, Reply, State};

handle_call({discover,ServiceId}, _From, State) ->
    Reply=lib_etcd_sd:discover(ServiceId),
    {reply, Reply, State};

handle_call({clean_up}, _From, State) ->
    Reply=lib_etcd_sd:clean_up(),
    {reply, Reply, State};


handle_call({create,ServiceId,Module,Node}, _From, State) ->
    Reply=lib_etcd_sd:create(ServiceId,Module,Node),
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
handle_info(clean_up, State) ->
    _Result=lib_etcd_sd:clean_up(),
    timer:send_after(?RefreshTime, clean_up),
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
%do_clean_up()->
%    timer:sleep(?
