%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>
-module(lib_etcd_zigbee_device).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("etcd_zigbee_device.hrl").

%% External exports

-export([create_table/0,create_table/2,add_node/2]).
-export([create/6,delete/1]).
-export([get_info/1,get_all/0,get/2,get_all_id/0]).
-export([do/1]).
-export([member/1]).
-export([git_clone_load/0]).

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
create(Name,ModelId,DeviceType,State,Type,Module)->
    Record=#?RECORD{
		    name=Name,
		    modelid=ModelId,
		    device_type=DeviceType,
		    state=State,
		    type=Type,
		    module=Module
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

delete(Name) ->
    F = fun() -> 
		mnesia:delete({?TABLE,Name})
		    
	end,
    mnesia:transaction(F).
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

member(Name)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Name])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

get_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{R#?RECORD.name,R#?RECORD.modelid,R#?RECORD.device_type,R#?RECORD.state,
      R#?RECORD.type,R#?RECORD.module}||R<-Z].

get_info(Name)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Name])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.name,
			    R#?RECORD.modelid,
			    R#?RECORD.device_type,
			    R#?RECORD.state,
			    R#?RECORD.type,
			    R#?RECORD.module}||R<-Z],
		   Info
	   end,
    Result.

get(Key,Name)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Name])),
    Result=case Z of
	       []->
		   {error,[eexist,Name,?MODULE,?LINE]};
	       [R] ->
		   case  Key of
		       name->
			   {ok,R#?RECORD.name};
		       modelid->
			   {ok,R#?RECORD.modelid};
		       state->
			   {ok,R#?RECORD.state};
		       type->
			   {ok,R#?RECORD.type};
		       device_type->
			   {ok,R#?RECORD.device_type};
		       module->
			   {ok,R#?RECORD.module};
		       Err ->
			   {error,['Key eexists',Err,Name,?MODULE,?LINE]}
		   end
	   end,
    Result.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.name||R<-Z].
    

%%--------------------------------------------------------------------
%% @doc
%%  
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
%%  
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
%%  
%% @end
%%--------------------------------------------------------------------

git_clone()->
    TempDirName=erlang:integer_to_list(os:system_time(microsecond),36)++".dir",
    ok=file:make_dir(TempDirName),
    GitDir=filename:join(TempDirName,?SpecDir),
    GitPath=?GitPathSpecs,
    os:cmd("rm -rf "++GitDir),    
    ok=file:make_dir(GitDir),
    GitResult=cmn_appl:git_clone_to_dir(node(),GitPath,GitDir),
    Result=case filelib:is_dir(GitDir) of
	       false->
		   {error,[failed_to_clone,GitPath,GitResult]};
	       true->
		   {ok,TempDirName,GitDir}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

from_file(ApplSpecDir)->
    {ok,FileNames}=file:list_dir(ApplSpecDir),
    from_file(FileNames,ApplSpecDir,[]).

from_file([],_,Acc)->
    Acc;		     
from_file([FileName|T],Dir,Acc)->
    NewAcc=case filename:extension(FileName) of
	       ?Extension->
		   FullFileName=filename:join(Dir,FileName),
		   case file:consult(FullFileName) of
			{error,Reason}->
			    [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc];
			{ok,[{zigbee_device,Map}]}->
			   Name=maps:get(<<"name">>,Map),
			   ModelId=maps:get(<<"modelid">>,Map),
			   DeviceType=maps:get(<<"device_type">>,Map),
			   State=maps:get(<<"state">>,Map),
			   Type=maps:get(<<"type">>,Map),
			   Module=maps:get(<<"module">>,Map),
			   case create(Name,ModelId,DeviceType,State,Type,Module) of
			       {atomic,ok}->
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
			   
