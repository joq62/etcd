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
-module(etcd_cluster_test).       
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(ClusterNameTest,"test_c50_1").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    Node=node(),
    ok=setup(Node),
    ok=read_specs_test(Node),
    ok=crudo_test(Node),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
crudo_test(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
read_specs_test(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    AllHosts=lists:sort(rpc:call(Node,etcd_cluster,all_clusters,[],5000)),
    true=lists:member( ?ClusterNameTest,AllHosts),

    {"test_c50_1","a","test_c50",[]}=rpc:call(Node,etcd_cluster,get_info,[?ClusterNameTest],5000),
    
    {ok,"a"}=rpc:call(Node,etcd_cluster,get_cookie_str,[?ClusterNameTest],5000),
    {ok,"test_c50"}=rpc:call(Node,etcd_cluster,get_deployment_spec,[?ClusterNameTest],5000),
    {ok,[]}=rpc:call(Node,etcd_cluster,get_deployment_records,[?ClusterNameTest],5000),
       

    {error,[eexist,"glurk",lib_etcd_cluster,_]}=rpc:call(Node,etcd_cluster,get_cookie_str,["glurk"],5000),
 
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    pong=rpc:call(Node,etcd,ping,[],5000),
   
    ok.