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
-module(etcd_deployment_record_test).       
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("etcd_cluster.hrl").
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
    ok=create_record_test(Node),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_record_test(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    AllHosts=lists:sort(rpc:call(Node,etcd_cluster,all_clusters,[],5000)),
    true=lists:member( ?ClusterNameTest,AllHosts),
    {"test_c50_1","a","test_c50",[]}=rpc:call(Node,etcd_cluster,get_info,[?ClusterNameTest],5000),
    {ok,CookieStr}=rpc:call(Node,etcd_cluster,get_cookie_str,[?ClusterNameTest],5000),
    {ok,DeploymentSpec}=rpc:call(Node,etcd_cluster,get_deployment_spec,[?ClusterNameTest],5000),
    {ok,DeploymentList}=rpc:call(Node,etcd_deployment,get_deployment_list,[DeploymentSpec],5000),
    [{"adder","c50"},{"control","c50"},{"divi","c50"},{"etcd","c50"}]=lists:sort(DeploymentList),
    Num=length(DeploymentList),
    4=Num,
    [
     {deployment_record,"a_1",'a_1@c50',"a_1","divi","c50"},
     {deployment_record,"a_2",'a_2@c50',"a_2","adder","c50"},
     {deployment_record,"a_3",'a_3@c50',"a_3","etcd","c50"},
     {deployment_record,"a_4",'a_4@c50',"a_4","control","c50"}
    ]=create_records(DeploymentList,Num,CookieStr,[]),
    ok.

create_records([],_Num,_CookieStr,Acc)->
    Acc;
create_records([{Provider,Host}|T],N,CookieStr,Acc) ->
    %NodeName= "CookieStr_N"
    % Node NodeName@HostName
    % Dir =NodeName
    NStr=integer_to_list(N),
    NodeName=CookieStr++"_"++NStr,
    Node=list_to_atom(NodeName++"@"++Host),
    R=#deployment_record{node_name=NodeName,
                         node=Node,
			 dir=NodeName,
			 provider=Provider,
			 host=Host},
    create_records(T,N-1,CookieStr,[R|Acc]).
    


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    pong=rpc:call(Node,etcd,ping,[],5000),
   
    ok.
