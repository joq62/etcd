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
-module(sys_boot_test).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(ClusterSpec,"test_c50_1").
-define(DeploymentSpec,"test_c50").
-define(HostSpec,"c50").
-define(CookieStr,"a").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    ok=boot_test(),
     
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
boot_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    %% Check dbase
    ClusterSpecs=etcd_cluster:all_clusters(),
    true=lists:member(?ClusterSpec,ClusterSpecs),
    DeploymentSpecs=etcd_deployment:all_deployments(),
    true=lists:member(?DeploymentSpec,DeploymentSpecs),
    HostSpecs=etcd_host:all_hosts(),
    true=lists:member(?HostSpec,HostSpecs),
    

    % Creted by sys_boot
    ok=etcd_lock:create(?ClusterSpec),
    ok=etcd_cluster_to_deploy:create(?ClusterSpec,node()),
    {ok,DeploymentRecords}=etcd_deployment_record:create_records(?ClusterSpec),
    ok=etcd_cluster:set_deployment_records(DeploymentRecords,?ClusterSpec),
    % 
    % Check by control
    {ok,ClusterSpec}=etcd_cluster_to_deploy:get_cluster_spec(),
    {ok,?CookieStr}=etcd_cluster:get_cookie_str(ClusterSpec),
    {ok,DeploymentRecords}=etcd_cluster:get_deployment_records(ClusterSpec),
    [{deployment_record,"a_1",a_1@c50,etcd,"a_1.provider_dir","etcd","c50"},
     {deployment_record,"a_2",a_2@c50,divi,"a_2.provider_dir","divi","c50"},
     {deployment_record,"a_3",a_3@c50,control,"a_3.provider_dir","control","c50"},
     {deployment_record,"a_4",a_4@c50,adder,"a_4.provider_dir","adder","c50"}
    ]=lists:sort(DeploymentRecords),
   
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok=application:start(log),
    pong=log:ping(),

    ok=application:start(etcd),
    pong=etcd:ping(),
    ok.
