%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>
-module(lib_etcd_infra).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("etcd_infra.hrl").

%% External exports

-export([create_table/0,create_table/2,add_node/2]).
-export([create/4,delete/1]).
-export([get_info/1,get_all/0,get/2,get/3,get_all_id/0]).
-export([do/1]).
-export([member/1]).
-export([git_clone_load/0]).

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
create(SpecId,ConnectNodes,CookieStr,NumWorkers)->
    Record=#?RECORD{
		    spec_id=SpecId,
		    connect_nodes=ConnectNodes,
		    cookie_str=CookieStr,
		    num_workers=NumWorkers
		   },
    F = fun() -> mnesia:write(Record) end,
    case mnesia:transaction(F) of
	{atomic,ok}->
	    ok;
	Reason->
	    {error,[Reason,?MODULE,?LINE]}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

delete(SpecId) ->
    F = fun() -> 
		mnesia:delete({?TABLE,SpecId})
		    
	end,
    mnesia:transaction(F).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

member(SpecId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec_id==SpecId])),
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
    [{R#?RECORD.spec_id,R#?RECORD.connect_nodes,R#?RECORD.cookie_str,R#?RECORD.num_workers}||R<-Z].

get_info(SpecId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec_id==SpecId])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.spec_id,R#?RECORD.connect_nodes,R#?RECORD.cookie_str,
			    R#?RECORD.num_workers}||R<-Z],
		   Info
	   end,
    Result.



get(num_workers,SpecId,HostName)->
    Result=case get(num_workers,SpecId) of
	       {error,Reason}->
		   {error,[Reason,?MODULE,?LINE]};
	       {ok,NumWorkersList} ->
		   case lists:keyfind(HostName,1,NumWorkersList) of
		       false->
			   {error,["eexist host",HostName,?MODULE,?LINE]};
		       {HostName,NumWorkers}->
			   {ok,NumWorkers}
		   end
	   end,
    Result;

get(ErrorKey,_SpecId,_HostName)->
    {error,['Key eexists',ErrorKey,?MODULE,?LINE]}.

		    
get(Key,SpecId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec_id==SpecId])),
    Result=case Z of
	       []->
		   {error,[eexist,SpecId,?MODULE,?LINE]};
	       [R] ->
		   case  Key of
		       spec_id->
			   {ok,R#?RECORD.spec_id};
		       connect_nodes->
			   {ok,R#?RECORD.connect_nodes};
		       cookie_str->
			   {ok,R#?RECORD.cookie_str};
		       num_workers->
			   {ok,R#?RECORD.num_workers};
		       Err ->
			   {error,['Key eexists',Err,Key,?MODULE,?LINE]}
		   end
	   end,
    Result.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.spec_id||R<-Z].
        
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    Result=case mnesia:transaction(F) of
	       {atomic, Val} ->
		   Val;
	       {error,Reason}->
		   {error,Reason}
	   end,
    Result.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

git_clone_load()->
    ok=create_table(),
    Result=case git_clone() of
	       {error,Reason}->
		   {error,Reason};
	       {ok,TempDirName,SpecDir}->
		   case from_file(SpecDir) of
		       {error,Reason}->
			   os:cmd("rm -rf "++TempDirName),	
			   {error,Reason};
		       LoadResult->
			   os:cmd("rm -rf "++TempDirName),	
			   LoadResult
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

git_clone()->
    TempDirName=erlang:integer_to_list(os:system_time(microsecond),36)++".dir",
    ok=file:make_dir(TempDirName),
    GitDir=filename:join(TempDirName,?SpecDir),
    GitPath=?GitPathSpecs,
    os:cmd("rm -rf "++GitDir),    
    ok=file:make_dir(GitDir),
    Result=case cmn_appl:git_clone_to_dir(node(),GitPath,GitDir) of
	       {error,Reason}->
		   {error,Reason};
	       GitResult->
		   case filelib:is_dir(GitDir) of
		       false->
			   {error,[failed_to_clone,GitPath,GitResult]};
		       true->
			   {ok,TempDirName,GitDir}
		   end
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

from_file(SpecDir)->
    {ok,FileNames}=file:list_dir(SpecDir),
    from_file(FileNames,SpecDir,[]).

from_file([],_,Acc)->
    Acc;		     
from_file([FileName|T],Dir,Acc)->
    NewAcc=case filename:extension(FileName) of
	       ?Extension->
		   FullFileName=filename:join(Dir,FileName),
		   case file:consult(FullFileName) of
			{error,Reason}->
			    [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc];
			{ok,[{infra_spec,Info}]}->
			   {spec_id,SpecId}=lists:keyfind(spec_id,1,Info),
			   {connect_nodes,ConnectNodes}=lists:keyfind(connect_nodes,1,Info),
			    {cookie_str,CookieStr}=lists:keyfind(cookie_str,1,Info),
			    {num_workers,NumWorkers}=lists:keyfind(num_workers,1,Info),
			   case create(SpecId,ConnectNodes,CookieStr,NumWorkers) of
				ok ->
				    [{ok,FileName}|Acc];
				{error,Reason}->
				    [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc]
			    end
		   end;
	       _ -> 
		   Acc
		   %[{error,[not_appl_spec_file,NotAnApplSpecFile,FileName,Dir,?MODULE,?LINE]}|Acc]
	   end,
    from_file(T,Dir,NewAcc).
			   
