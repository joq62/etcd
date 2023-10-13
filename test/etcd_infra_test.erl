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
-module(etcd_infra_test).       
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(InfraTest,"basic").

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
    
    AllInfra=lists:sort(rpc:call(Node,etcd_infra,all_infra,[],5000)),
    io:format("AllInfra ~p~n",[{AllInfra,?MODULE,?FUNCTION_NAME}]),
    true=lists:member(?InfraTest,AllInfra),

    {
     "basic",
     ['control_a@c50','control_a@c200','control_a@c201','control_a@c202'],
     "a",
     [{"c50",9},{"c200",10},{"c201",11},{"c202",12}]
    }=rpc:call(Node,etcd_infra,get_info,[?InfraTest],5000),
    
    {ok,['control_a@c50','control_a@c200','control_a@c201','control_a@c202']}=rpc:call(Node,etcd_infra,get_connect_nodes,[?InfraTest],5000),
    {ok,"a"}=rpc:call(Node,etcd_infra,get_cookie_str,[?InfraTest],5000),
    {ok,[{"c50",9},{"c200",10},{"c201",11},{"c202",12}]}=rpc:call(Node,etcd_infra,get_num_workers,[?InfraTest],5000),
    {ok,10}=rpc:call(Node,etcd_infra,get_num_workers,[?InfraTest,"c200"],5000),
   
    {error,[eexist,"glurk",lib_etcd_infra,_]}=rpc:call(Node,etcd_infra,get_cookie_str,["glurk"],5000),
 
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
