%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(etcd_zigbee_device).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
 

%% API

-export([
	 all_devices/0,
	 get_info/1,
	 get_modelid/1,
	 get_state/1,
	 get_type/1,
	 get_device_type/1,
	 get_module/1,
	 member/1,

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
-spec all_devices() -> ListOfDeviceNames :: string().

all_devices()->
    gen_server:call(?SERVER, {all_devices},infinity).
    

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec member(Name :: string()) -> Member :: boolean() | {error, Error :: term()}.

member(Name)->
    gen_server:call(?SERVER, {member,Name},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_info(Name :: string()) -> {ok,DeviceInfo :: term()} | {error, Error :: term()}.

get_info(Name)->
    gen_server:call(?SERVER, {get_info,Name},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get ip adress from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_modelid(Name :: string()) -> {ok,ModelId :: string()} | {error, Error :: term()}.

get_modelid(Name)->
    gen_server:call(?SERVER, {get_modelid,Name},infinity).
%%--------------------------------------------------------------------
%% @doc
%% get ip adress from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_state(Name :: string()) -> {ok,StateInfo :: term()} | {error, Error :: term()}.

get_state(Name)->
    gen_server:call(?SERVER, {get_state,Name},infinity).
%%--------------------------------------------------------------------
%% @doc
%% get ip adress from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_type(Name :: string()) -> {ok,Type :: string()} | {error, Error :: term()}.

get_type(Name)->
    gen_server:call(?SERVER, {get_type,Name},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get ip adress from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_device_type(Name :: string()) -> {ok,Type :: string()} | {error, Error :: term()}.

get_device_type(Name)->
    gen_server:call(?SERVER, {get_device_type,Name},infinity).
%%--------------------------------------------------------------------
%% @doc
%% get ip adress from host HostName
%% @end
%%--------------------------------------------------------------------
-spec get_module(Name :: string()) -> {ok,Module :: atom()} | {error, Error :: term()}.

get_module(Name)->
    gen_server:call(?SERVER, {get_module,Name},infinity).

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
    case lists:delete(node(),sd:get_node(etcd)) of
	[]->
	    ok=lib_etcd_zigbee_device:create_table(),    
	    SpecList=lib_etcd_zigbee_device:git_clone_load(),
	    Ok_SpecList=[X||{ok,X}<-SpecList],
	    FailedToCreate=[X||{error,X}<-SpecList],
	    ?LOG_NOTICE("Successfully created  ",[Ok_SpecList]),
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
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({all_devices}, _From, State) ->
    Reply=lib_etcd_zigbee_device:get_all_id(),
    {reply, Reply, State};


handle_call({member,Name}, _From, State) ->
    Reply=lib_etcd_zigbee_device:member(Name),
    {reply, Reply, State};

handle_call({get_info,Name}, _From, State) ->
    Reply=lib_etcd_zigbee_device:get_info(Name),
    {reply, Reply, State};

handle_call({get_modelid,Name}, _From, State) ->
    Reply=lib_etcd_zigbee_device:get(modelid,Name),
    {reply, Reply, State};

handle_call({get_state,Name}, _From, State) ->
    Reply=lib_etcd_zigbee_device:get(state,Name),
    {reply, Reply, State};

handle_call({get_type,Name}, _From, State) ->
    Reply=lib_etcd_zigbee_device:get(type,Name),
    {reply, Reply, State};

handle_call({get_device_type,Name}, _From, State) ->
    Reply=lib_etcd_zigbee_device:get(device_type,Name),
    {reply, Reply, State};

handle_call({get_module,Name}, _From, State) ->
    Reply=lib_etcd_zigbee_device:get(module,Name),
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
