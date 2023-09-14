%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(etcd_lock).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
 

%% API

-export([
	 create/1,
	 create/2,
	 all_locks/0,
	 get_info/1,
	 
	 try_lock/1,
	 try_lock/2,
	 unlock/2,
	 is_open/1,
	 is_open/2,
	 
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
-spec create(LockId :: atom()) -> ok | {error, Error :: term()}.

create(LockId)->
    create(LockId,0).
%%--------------------------------------------------------------------
%% @doc
%% Creates a new instance 
%% @end
%%--------------------------------------------------------------------
-spec create(Lock :: atom(),Time :: integer()) -> ok | {error, Error :: term()}.
create(Lock,Time)->
    gen_server:call(?SERVER, {create,Lock,Time},infinity).
    
%%--------------------------------------------------------------------
%% @doc
%% get all locks name that are stored in dbase
%% @end
%%--------------------------------------------------------------------
-spec all_locks() -> ListOfLocks :: term().

all_locks()->
    gen_server:call(?SERVER, {all_locks},infinity).
    

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to lock Lock 
%% @end
%%--------------------------------------------------------------------
-spec get_info(Lock :: atom()) -> {ok,LockInfo :: term()} | {error, Error :: term()}.

get_info(Lock)->
    gen_server:call(?SERVER, {get_info,Lock},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Tries to take lead by locking 
%% @end
%%--------------------------------------------------------------------
-spec try_lock(Lock :: atom()) -> {ok,Transaction :: integer()} | locked | {error, Error :: term()}.

try_lock(Lock)->
    gen_server:call(?SERVER, {try_lock,Lock},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Tries to take lead by locking 
%% @end
%%--------------------------------------------------------------------
-spec try_lock(Lock :: atom(), LockTimeOut :: integer()) -> {ok,Transaction :: integer()} | locked | {error, Error :: term()}.

try_lock(Lock,LockTimeOut)->
    gen_server:call(?SERVER, {try_lock,Lock,LockTimeOut},infinity).

%%--------------------------------------------------------------------
%% @doc
%% unlock 
%% @end
%%--------------------------------------------------------------------
-spec unlock(Lock :: atom(), Transaction :: integer()) -> ok | {error, Error :: term()}.

unlock(Lock,Transaction)->
    gen_server:call(?SERVER, {unlock,Lock,Transaction},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Check if lock is open or locked 
%% @end
%%--------------------------------------------------------------------
-spec is_open(Lock :: atom()) -> true | false | {error, Error :: term()}.

is_open(Lock)->
    gen_server:call(?SERVER, {is_open,Lock},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Check if lock is open or locked 
%% @end
%%--------------------------------------------------------------------
-spec is_open(Lock :: atom(),LockTimeOut :: integer()) -> true | false  | {error, Error :: term()}.

is_open(Lock,LockTimeOut)->
    gen_server:call(?SERVER, {is_open,Lock,LockTimeOut},infinity).

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
    case lists:delete(node(),rd:fetch_resources(etcd)) of
	[]->
	    ok=lib_etcd_lock:create_table();
	_ ->
	    ok
    end,
    
    ?LOG_NOTICE("Server started  ",[]),
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_call({all_locks}, _From, State) ->
    Reply=lib_etcd_lock:get_all_id(),
    {reply, Reply, State};

handle_call({get_info,Lock}, _From, State) ->
    Reply=lib_etcd_lock:get_info(Lock),
    {reply, Reply, State};

handle_call({try_lock,Lock}, _From, State) ->
    Reply=lib_etcd_lock:try_lock(Lock),
    {reply, Reply, State};

handle_call({try_lock,Lock,LockTimeOut}, _From, State) ->
    Reply=lib_etcd_lock:try_lock(Lock,LockTimeOut),
    {reply, Reply, State};

handle_call({is_open,Lock}, _From, State) ->
    Reply=lib_etcd_lock:is_open(Lock),
    {reply, Reply, State};

handle_call({is_open,Lock,LockTimeOut}, _From, State) ->
    Reply=lib_etcd_lock:is_open(Lock,LockTimeOut),
    {reply, Reply, State};


handle_call({unlock,Lock,Transaction}, _From, State) ->
    Reply=lib_etcd_lock:unlock(Lock,Transaction),
    {reply, Reply, State};

handle_call({create,Lock}, _From, State) ->
    Reply=lib_etcd_lock:create(Lock),
    {reply, Reply, State};

handle_call({create,Lock,Time}, _From, State) ->
    Reply=lib_etcd_lock:create(Lock,Time),
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
