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
-module(all).      
 
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
   
    ok=setup(),

    
       % dbetcd
    
%    ok=etcd_infra_test:start(),
%    ok=etcd_zigbee_device_test:start(),
    ok=etcd_host_test:start(node()),
%    ok=etcd_application_test:start(node()),
%    ok=etcd_lock_test:start(node()),
%    ok=etcd_deployment_test:start(),
%    ok=etcd_cluster_test:start(),
%    ok=etcd_paas_config_test:start(),
%    ok=dist_test:start(),
   
                 
   
    io:format("Test OK !!! ~p~n",[?MODULE]),
%    timer:sleep(2000),
%    init:stop(),
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
    ok=application:start(rd),
    pong=rd:ping(),    
    ok=application:start(etcd),
    pong=etcd:ping(),
    ok.
