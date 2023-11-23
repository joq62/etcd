%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>
-module(lib_etcd_paas_config).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("etcd_paas_config.hrl").


-define(LockTimeOut,2*60*1000).
%% External exports

-export([create_table/0,create_table/2,add_node/2]).
-export([create/2,delete/0]).
-export([get_info/0,get_all/0,get/1]).
-export([do/1]).


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {type,set}
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
create(ClusterSpec,Lock) ->
    F = fun() ->
		Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
		case Z of
		    []->
			Record=#?RECORD{
					cluster_spec=ClusterSpec,
					lock=Lock
				       },		
			mnesia:write(Record);
		    _->
			mnesia:abort(?TABLE) 
		end
	end,
    case mnesia:transaction(F) of
	{atomic,ok}->
	    ok;
	Reason ->
	    {error,["Already initiated",Reason,?MODULE,?LINE]}
    end.


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

delete() ->
    F = fun() -> 
	Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
		case Z of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
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

get_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{R#?RECORD.cluster_spec,R#?RECORD.lock}||R<-Z].

get_info()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.cluster_spec,
			    R#?RECORD.lock}||R<-Z],
		   Info
	   end,
    Result.

get(Key)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    Result=case Z of
	       []->
		   {error,[eexist,?MODULE,?LINE]};
	       [R] ->
		   case  Key of
		       lock->
			   {ok,R#?RECORD.lock};
		       cluster_spec->
			   {ok,R#?RECORD.cluster_spec};
		       Err ->
			   {error,['Key eexists',Err,?MODULE,?LINE]}
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
