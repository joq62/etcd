%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(etcd_sd_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------


%% External exports
-export([start/0]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    ok=setup(),
    ok=lock_test_1(),
 
    io:format("End testing  SUCCESS!! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
 %   init:stop(),
 %   timer:sleep(3000),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
lock_test_1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    S1={s1,s1,node1},
    S11={s1,s1,node2},
    S2={s2,s3,node1},
    SL=[S1,S11,S2],

    [
     {ok,{sd,s1,s1,node1,_}},
     {ok,{sd,s1,s1,node2,_}},
     {ok,{sd,s2,s3,node1,_}}
    ]=[etcd_sd:register(ServiceId,Module,Node)||{ServiceId,Module,Node}<-SL],
    
    [{node1,s1},{node2,s1}]=etcd_sd:discover(s1),
    [{node1,s3}]=etcd_sd:discover(s2),
    []=etcd_sd:discover(s3),

    timer:sleep(1000),
    {ok,{sd,s1,s1,node2,_}}=etcd_sd:register(s1,s1,node2),
    timer:sleep(1800),


    [{node2,s1}]=etcd_sd:discover(s1),
    []=etcd_sd:discover(s2),
    []=etcd_sd:discover(s3),
    
    
    %% 
    
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
    
    pong=etcd_sd:ping(),
    ok=lib_etcd_sd:create_table(),

    ok.
