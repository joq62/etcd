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
-module(etcd_provider_test).        
 
-export([start/1]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(ProviderNameTest,"adder").

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
    
    AllIds=lists:sort(rpc:call(Node,etcd_provider,all_providers,[],5000)),
   % glurk=AllIds,
    true=lists:member( ?ProviderNameTest,AllIds),

    {
     "adder","0.1.0",adder,
     " ","https://github.com/joq62/adder.git"
    }=rpc:call(Node,etcd_provider,get_info,[?ProviderNameTest],5000),
    
    {ok,"0.1.0"}=rpc:call(Node,etcd_provider,get_vsn,[?ProviderNameTest],5000),
    {ok,adder}=rpc:call(Node,etcd_provider,get_app,[?ProviderNameTest],5000),
    {ok," "}=rpc:call(Node,etcd_provider,get_erl_args,[?ProviderNameTest],5000),
    {ok,"https://github.com/joq62/adder.git"}=rpc:call(Node,etcd_provider,get_git_path,[?ProviderNameTest],5000),
      

    {error,[eexist,"glurk",lib_provider,_]}=rpc:call(Node,etcd_provider,get_vsn,["glurk"],5000),
 
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
