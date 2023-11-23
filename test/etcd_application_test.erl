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
-module(etcd_application_test).        
 
-export([start/1]). 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(ApplicationNameTest,"adder").

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
    
    AllIds=lists:sort(rpc:call(Node,etcd_application,all_applications,[],5000)),
    true=lists:member( ?ApplicationNameTest,AllIds),

    {
     "adder","0.1.0",adder,
     " ","https://github.com/joq62/adder.git"
    }=rpc:call(Node,etcd_application,get_info,[?ApplicationNameTest],5000),
    
    {ok,"0.1.0"}=rpc:call(Node,etcd_application,get_vsn,[?ApplicationNameTest],5000),
    {ok,adder}=rpc:call(Node,etcd_application,get_app,[?ApplicationNameTest],5000),
    {ok," "}=rpc:call(Node,etcd_application,get_erl_args,[?ApplicationNameTest],5000),
    {ok,"https://github.com/joq62/adder.git"}=rpc:call(Node,etcd_application,get_git_path,[?ApplicationNameTest],5000),
      

    {error,[eexist,"glurk",lib_etcd_application,_]}=rpc:call(Node,etcd_application,get_vsn,["glurk"],5000),
 
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
