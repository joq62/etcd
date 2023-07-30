%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(etcd_lock_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(Lock,etcd_lock).

%% External exports
-export([start/1]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    ok=setup(),
    ok=lock_test_1(),
 
    io:format("End testing  SUCCESS!! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
%    init:stop(),
%    timer:sleep(3000),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
lock_test_1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    %% db_lock timeout set to
    LockTimeOut=3000,   %% 3 Seconds
    [{error,[eexists,glurk]}]=etcd_lock:try_lock(glurk,LockTimeOut),

    {ok,TransAction_1}=etcd_lock:try_lock(?Lock,LockTimeOut),
    timer:sleep(1*1000),
    locked=etcd_lock:try_lock(?Lock,LockTimeOut),

    {error,[eexists,glurk]}=etcd_lock:unlock(glurk,TransAction_1),
    {error,["eexists Transactions",glurk,_]}=etcd_lock:unlock(?Lock,glurk),

    ok=etcd_lock:unlock(?Lock,TransAction_1),
    {ok,TransAction_2}=etcd_lock:try_lock(?Lock,LockTimeOut),
    locked=etcd_lock:try_lock(?Lock,LockTimeOut),

    timer:sleep(3*1000),
    {ok,TransAction_3}=etcd_lock:try_lock(?Lock,LockTimeOut),
    locked=etcd_lock:try_lock(?Lock,LockTimeOut),
  
%    io:format("TransActionsId_1 ~p~n",[{TransActionsId_1,?MODULE,?FUNCTION_NAME,?LINE}]),
%    io:format("TransActionsId_2 ~p~n",[{TransActionsId_2,?MODULE,?FUNCTION_NAME,?LINE}]),
%    io:format("TransActionsId_3 ~p~n",[{TransActionsId_3,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok. 
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=etcd_lock:create(?Lock),
    [?Lock]=etcd_lock:all_locks(),    
    {?Lock,na,0,na}=etcd_lock:get_info(?Lock),
    
    ok.
