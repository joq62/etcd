%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>
-module(lib_etcd_lock).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("etcd_lock.hrl").


-define(LockTimeOut,2*60*1000).
%% External exports

-export([create_table/0,create_table/2,add_node/2]).
-export([create/1,create/2,delete/1]).
-export([get_info/1,get_all/0,get/2,get_all_id/0]).
-export([do/1]).
-export([member/1]).
-export([try_lock/1,try_lock/2,unlock/2,is_open/1]).


%%--------------------------------------------------------------------
%% @doc
%% @spec
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
%% @spec
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
%% @spec
%% @end
%%--------------------------------------------------------------------
create(Lock) ->
    create(Lock,0).
create(Lock,Time) ->
    F = fun() ->
		Record=#?RECORD{lock=Lock,
				transaction=na,
				time=Time,
				status=na},		
		mnesia:write(Record) end,
    case mnesia:transaction(F) of
	{atomic,ok}->
	    ok;
	Reason ->
	    {error,[Reason,?MODULE,?LINE]}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

delete(Lock) ->
    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Lock}),
			    X#?RECORD.lock==Lock],
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

member(Lock)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.lock==Lock])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

get_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{R#?RECORD.lock,R#?RECORD.transaction,R#?RECORD.time,
      R#?RECORD.status}||R<-Z].

get_info(Lock)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.lock==Lock])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.lock,
			    R#?RECORD.transaction,
			    R#?RECORD.time,
			    R#?RECORD.status}||R<-Z],
		   Info
	   end,
    Result.

get(Key,Lock)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.lock==Lock])),
    Result=case Z of
	       []->
		   {error,[eexist,Lock,?MODULE,?LINE]};
	       [R] ->
		   case  Key of
		       lock->
			   {ok,R#?RECORD.lock};
		       transaction->
			   {ok,R#?RECORD.transaction};
		       time->
			   {ok,R#?RECORD.time};
		       status->
			   {ok,R#?RECORD.status};
		       Err ->
			   {error,['Key eexists',Err,Lock,?MODULE,?LINE]}
		   end
	   end,
    Result.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.lock||R<-Z].
   

try_lock(Lock)->
    try_lock(Lock,?LockTimeOut).
try_lock(Lock,LockTimeOut)->
    F=fun()->
	      case mnesia:read({?TABLE,Lock}) of
		  []->
		      {mnesia:abort([{error,[eexists,Lock]}])};
		  [LockInfo] ->
		      
 		      CurrentTime=erlang:system_time(millisecond),
		      LockTime=LockInfo#?RECORD.time,
		      TimeDiff=CurrentTime-LockTime,
		      if
			  TimeDiff > LockTimeOut-> %% Caller didnt un_lock or it timeout
			      Transaction=erlang:system_time(microsecond),
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime,
							 status=locked,
							 transaction=Transaction},
			      {mnesia:write(LockInfo1),Transaction};
			  TimeDiff == LockTimeOut->
			      Transaction=erlang:system_time(microsecond),
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime,
							 status=locked,
							 transaction=Transaction},
			      {mnesia:write(LockInfo1),Transaction};
			  TimeDiff < LockTimeOut->
			       mnesia:abort(locked)
		      end
	      end
      end,
    Result=case mnesia:transaction(F) of
	       {atomic,{ok,Transaction}}->
		   {ok,Transaction};
	       {aborted,Reason}->
		   Reason
	   end,
    Result.

unlock(Lock,Transaction)->
    F=fun()->
	      case mnesia:read({?TABLE,Lock}) of
		  []->
		      {mnesia:abort({error,[eexists,Lock]})};
		  [LockInfo] ->
		      CurrentTransaction=LockInfo#?RECORD.transaction,
		      case Transaction==CurrentTransaction of
			  false->
			      mnesia:abort({error,["eexists Transactions",Transaction,CurrentTransaction]});
			  true->
			      LockInfo1=LockInfo#?RECORD{
							 time=0,
							 status=unlocked,
							 transaction=na},
			      mnesia:write(LockInfo1)
		      end
	      end
      end,
    Result=case mnesia:transaction(F) of
	       {atomic,ok}->
		   ok;
	       {aborted,Reason}->
		   Reason
	   end,
    Result.
 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
   
is_open(Lock)->
    is_open(Lock,?LockTimeOut).
is_open(Lock,LockTimeOut)->
    F=fun()->
	      case mnesia:read({?TABLE,Lock}) of
		  []->
		      {mnesia:abort({error,[eexists,Lock]})};
		  [LockInfo] ->
		      
		      CurrentTime=erlang:system_time(millisecond),
		      LockTime=LockInfo#?RECORD.time,
		      TimeDiff=CurrentTime-LockTime,
		      if
			  TimeDiff > LockTimeOut->
			      
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime},
			      mnesia:write(LockInfo1);
			  TimeDiff == LockTimeOut->
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime},
			      mnesia:write(LockInfo1);
			  TimeDiff < LockTimeOut->
			       mnesia:abort(Lock)
		      end
	      end
      end,
    Result=case mnesia:transaction(F) of
	       {atomic,ok}->
		   true;
	       {aborted,Lock}->
		   false;
	       {aborted,{error,[eexists,Lock]}}->
		   {error,[eexists,Lock]}
	   end,
    Result.
		



do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
