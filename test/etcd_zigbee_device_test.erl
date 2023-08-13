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
-module(etcd_zigbee_device_test).       
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(NameTest,<<"light_color_joakim">>).

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
    
    AllDevices=lists:sort(rpc:call(Node,etcd_zigbee_device,all_devices,[],5000)),
    true=lists:member( ?NameTest,AllDevices),

    {
     <<"light_color_joakim">>,
     <<"TRADFRI bulb E27 CWS 806lm">>,
     <<"lights">>,
     [<<"bri">>,<<"colormode">>,<<"ct">>,<<"xy">>,<<"effect">>,<<"hue">>,<<"alert">>,<<"on">>,<<"reachable">>],
     <<"Extended color light">>,
     tradfri_bulb_E27_cws_806lm
    }=rpc:call(Node,etcd_zigbee_device,get_info,[?NameTest],5000),
    
    {ok,<<"TRADFRI bulb E27 CWS 806lm">>}=rpc:call(Node,etcd_zigbee_device,get_modelid,[?NameTest],5000),
    {ok, [<<"bri">>,<<"colormode">>,<<"ct">>,<<"xy">>,<<"effect">>,<<"hue">>,<<"alert">>,<<"on">>,<<"reachable">>]}=rpc:call(Node,etcd_zigbee_device,get_state,[?NameTest],5000),
    {ok,<<"Extended color light">>}=rpc:call(Node,etcd_zigbee_device,get_type,[?NameTest],5000),
    {ok,<<"lights">>}=rpc:call(Node,etcd_zigbee_device,get_device_type,[?NameTest],5000),
    {ok,tradfri_bulb_E27_cws_806lm}=rpc:call(Node,etcd_zigbee_device,get_module,[?NameTest],5000),
  
   

    {error,[eexist,"glurk",lib_etcd_zigbee_device,_]}=rpc:call(Node,etcd_zigbee_device,get_module,["glurk"],5000),
 
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
