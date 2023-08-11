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
-define(RecordTest,{deployment_record,"a_1",'a_1@c50',log,"a_1.provider_dir","log","c50"}).

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
    
    {ok,DeploymentRecords}=rpc:call(Node,etcd_deployment_record,create_records,[?ClusterNameTest],5000),
    {ok,L}=rpc:call(Node,etcd_cluster,get_deployment_records,[?ClusterNameTest],5000),
    DeploymentRecords=L,
  %  stop=io:format("DeploymentRecords ~p~n",[{DeploymentRecords,?MODULE,?FUNCTION_NAME,?LINE}]),
    true=lists:member(?RecordTest,DeploymentRecords),
    {ok,"a_1"}=etcd_deployment_record:get_node_name(?RecordTest),
    {ok,a_1@c50}=etcd_deployment_record:get_node(?RecordTest),
    {ok,log}=etcd_deployment_record:get_app(?RecordTest),
    {ok,"a_1.provider_dir"}=etcd_deployment_record:get_dir(?RecordTest),
    {ok,"log"}=etcd_deployment_record:get_provider(?RecordTest),
    {ok,"c50"}=etcd_deployment_record:get_host(?RecordTest),
    
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
read_specs_test(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    AllHosts=lists:sort(rpc:call(Node,etcd_cluster,all_clusters,[],5000)),
    true=lists:member(?ClusterNameTest,AllHosts),
    
    {"test_c50_1","a","test_c50_1",[]}=rpc:call(Node,etcd_cluster,get_info,[?ClusterNameTest],5000),
    
    {ok,"a"}=rpc:call(Node,etcd_cluster,get_cookie_str,[?ClusterNameTest],5000),
    {ok,"test_c50_1"}=rpc:call(Node,etcd_cluster,get_deployment_spec,[?ClusterNameTest],5000),
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
