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
-module(cluster_test).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(ClusterSpec,"test_local_cluster").
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
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
read_specs_test(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    AllClusterSpecs=lists:sort(rpc:call(Node,db_cluster_spec,get_all_id,[],5000)),
   
    true=lists:member(?ClusterSpec,AllClusterSpecs),

    {
     ?ClusterSpec,"test_c50","a"
    }=rpc:call(Node,db_cluster_spec,read,[?ClusterSpec],5000),
    
    SpecId="cluster_to_deploy",
    {atomic,ok}=db_cluster:create(SpecId,?ClusterSpec),
    
    [SpecId]==db_cluster:get_all_id(),
    {ok,?ClusterSpec}=db_cluster:read(cluster_spec,SpecId),
    {atomic,ok}=db_cluster:create(SpecId,another),
    {ok,another}=db_cluster:read(cluster_spec,SpecId),
    
    
    
    {error,[eexist,"glurk",db_cluster,_]}=rpc:call(Node,db_cluster,read,[cluster_spec,"glurk"],5000),
    {error,[eexist,"test_local_cluster",db_cluster,_]}=rpc:call(Node,db_cluster,read,[glurk,?ClusterSpec],5000),
 
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
