%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(etcd_deployment_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(TestDeployment,"test_c50").
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
    ok=read_specs_test(),
 
    io:format("End testing  SUCCESS!! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
%    init:stop(),
%    timer:sleep(3000),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
read_specs_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    AllDepSpecs=lists:sort(etcd_deployment:all_deployments()),
    true=lists:member(?TestDeployment,AllDepSpecs),
    
    {
     "test_c50",
     [
      {"control","c50"},{"etcd","c50"},
      {"adder","c50"},{"divi","c50"}
     ]
    }=etcd_deployment:get_info(?TestDeployment),
    
    {ok,
     [
      {"control","c50"},{"etcd","c50"},
      {"adder","c50"},{"divi","c50"}
     ]
    }=etcd_deployment:get_deployment_list(?TestDeployment),
    
    {error,[eexist,"glurk",lib_etcd_deployment,_]}=etcd_deployment:get_deployment_list("glurk"),
    
 
    ok. 
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

   
    ok.
