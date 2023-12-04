%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>
-module(lib_etcd_sd).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include_lib("stdlib/include/qlc.hrl").
-include("etcd_sd.hrl").


%% External exports

-export([create_table/0,create_table/2,add_node/2]).
-export([create/3,delete/1]).
-export([get_info/2,get_all/0,get/2,get_all_id/0]).
-export([do/1]).
%%
-export([register/3,unregister/2,discover/1,clean_up/0]).


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {type,bag}
				]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList,StorageType)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {StorageType,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

add_node(Node,StorageType)->
    Result=case mnesia:change_config(extra_db_nodes, [Node]) of
	       {ok,[Node]}->
		   mnesia:add_table_copy(schema, node(),StorageType),
		   mnesia:add_table_copy(?TABLE, node(), StorageType),
		   Tables=mnesia:system_info(tables),
		   mnesia:wait_for_tables(Tables,20*1000);
	       Reason ->
		   Reason
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
create(ServiceId,Module,Node) ->
    Time=os:system_time(millisecond),
    F = fun() ->
		Record=#?RECORD{
				service_id=ServiceId,
				module=Module,
				node=Node,
				time=Time
			       },		
		mnesia:write(Record) end,
    case mnesia:transaction(F) of
	{atomic,ok}->
	    ok;
	Reason ->
	    {error,[Reason,?MODULE,?LINE]}
    end.


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
delete(ServiceId) ->
    F = fun() -> 
		mnesia:delete({?TABLE,ServiceId})
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

get_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{R#?RECORD.service_id,
      R#?RECORD.module,
      R#?RECORD.node,
      R#?RECORD.time}||R<-Z].


get_info(ServiceId,Node)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.service_id==ServiceId,
		     X#?RECORD.node==Node])),
    Result=case Z of
	       []->
		   {error,[error,"Not registered ServiceId,Node ",ServiceId,Node]};
	       _->
		   [Info]=[{R#?RECORD.service_id,
			    R#?RECORD.module,
			    R#?RECORD.node,
			    R#?RECORD.time}||R<-Z],
		   Info
	   end,
    Result.

get(Key,ServiceId)->
    {error,[error,"Not applicable key ServiceId",Key,ServiceId,?MODULE,?LINE]}. 


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.service_id||R<-Z].
   


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
register(ServiceId,Module,Node)->
    F=fun()->
	      Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
			       X#?RECORD.service_id==ServiceId,
			       X#?RECORD.node==Node])),
	      
	      Time=os:system_time(millisecond),
	      NewRecord= #?RECORD{service_id=ServiceId,
				  module=Module,
				  node=Node,
				  time=Time
				 },
	      case Z of
		  []-> %% First time registration
		    no_ation;
		  [R]->
		      mnesia:delete_object(?TABLE,R,write)
	      end,		      
	      mnesia:write(NewRecord),
	      {ok,NewRecord}
      end,
    case mnesia:transaction(F) of
	{atomic,{ok,NewRecord}}->
	    {ok,NewRecord};
	{aborted,Reason}->
	    Reason
    end.
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
unregister(ServiceId,Node)->
    F=fun()->
	      Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
			       X#?RECORD.service_id==ServiceId,
			       X#?RECORD.node==Node])),
	      case Z of
		  []-> %% First time registration
		      mnesia:abort({error,[error,"Not registered ServiceId,Node ",ServiceId,Node]});
		  [R]->
		      mnesia:delete_object(?TABLE,R,write),
		      {ok,R}
	      end
      end,
    case mnesia:transaction(F) of
	{atomic,{ok,DeletedRecord}}->
	    {ok,DeletedRecord};
	{aborted,Reason}->
	    Reason
    end.
		  
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
discover(ServiceId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.service_id==ServiceId])),
    Result=case Z of
	       []->
		   [];
	       
	       Z->
		   [{R#?RECORD.node,
		     R#?RECORD.module}||R<-Z]
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
clean_up()->
    F=fun()->
	      Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
	      ServicesToDelete=services_to_delete(Z,[]),
	      [mnesia:delete_object(?TABLE,R,write)||R<-ServicesToDelete]
      end,
    Result=case mnesia:transaction(F) of
	       {atomic,DeleteResult}->
		   {ok,DeleteResult};
	       {aborted,Reason}->
		   Reason
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
services_to_delete([],Acc)->
    Acc;
services_to_delete([R|T],Acc)->
    CurrentTime=erlang:system_time(millisecond),
    Time=R#?RECORD.time,
    Diff=CurrentTime-Time,
    NewAcc=if
	       Diff>?RefreshTime->
		   [R|Acc];
	       Diff==?RefreshTime->
		   [R|Acc];
	       Diff<?RefreshTime->
		   Acc
	   end,
    services_to_delete(T,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

