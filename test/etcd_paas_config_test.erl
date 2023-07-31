%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(etcd_paas_config_test).       
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=crudo_test(),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
crudo_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    []=etcd_paas_config:get_info(),
    ok=etcd_paas_config:create("cluster_spec",test_lock),
    {ok,"cluster_spec"}=etcd_paas_config:get_cluster_spec(),
    {ok,test_lock}=etcd_paas_config:get_lock(),
    {
     error,["Already initiated",{aborted,paas_config},lib_etcd_paas_config,_]
    }=etcd_paas_config:create("cluster_spec",test_lock),  
    ok=etcd_paas_config:delete(),   
    ok=etcd_paas_config:create("cluster_spec_2",test_lock_2),
    {ok,"cluster_spec_2"}=etcd_paas_config:get_cluster_spec(),
    {ok,test_lock_2}=etcd_paas_config:get_lock(),

    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    Node=node(),
    pong=rpc:call(Node,etcd,ping,[],5000),
   
    ok.
