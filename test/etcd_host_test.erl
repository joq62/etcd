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
-module(etcd_host_test).       
 
-export([start/1]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(HostNameTest,"c50").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

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
    
    AllHosts=lists:sort(rpc:call(Node,etcd_host,all_hosts,[],5000)),
    true=lists:member( ?HostNameTest,AllHosts),

    {
     "c50","172.26.158.249",22,"joq62","festum01",[]
    }=rpc:call(Node,etcd_host,get_info,[?HostNameTest],5000),
    
    {ok,"172.26.158.249"}=rpc:call(Node,etcd_host,get_ip,[?HostNameTest],5000),
    {ok,22}=rpc:call(Node,etcd_host,get_port,[?HostNameTest],5000),
    {ok,"joq62"}=rpc:call(Node,etcd_host,get_user,[?HostNameTest],5000),
    {ok,"festum01"}=rpc:call(Node,etcd_host,get_passwd,[?HostNameTest],5000),
    {ok,[]}=rpc:call(Node,etcd_host,get_appl_config,[ ?HostNameTest],5000),
   

    {error,[eexist,"glurk",lib_db_host,_]}=rpc:call(Node,etcd_host,get_port,["glurk"],5000),
 
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
