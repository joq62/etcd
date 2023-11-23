%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>
-module(lib_etcd_cluster).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("etcd_cluster.hrl").

%% External exports

-export([create_table/0,create_table/2,add_node/2]).
-export([create/1,delete/1]).
-export([get_info/1,get_all/0,get/2,get_all_id/0]).
-export([set/3]).
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
create(ClusterName)->
    Record=#?RECORD{
		    name=ClusterName,
		    cookie_str=undefined,
		    deployment_spec=undefined,
		    deployment_records=undefined
		   },
    F = fun() -> mnesia:write(Record) end,
    case mnesia:transaction(F) of
	{atomic,ok}->
	    ok;
	Reason->
	    {error,[Reason,?MODULE,?LINE]}
    end.

create(ClusterName,CookieStr,DeploymentSpec,DeploymentRecords)->
    Record=#?RECORD{
		    name=ClusterName,
		    cookie_str=CookieStr,
		    deployment_spec=DeploymentSpec,
		    deployment_records=DeploymentRecords
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).


%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

delete(ClusterName) ->
    F = fun() -> 
		mnesia:delete({?TABLE,ClusterName})
		    
	end,
    mnesia:transaction(F).
%%--------------------------------------------------------------------
%% @doc
%%  
%% @end
%%--------------------------------------------------------------------

member(ClusterName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==ClusterName])),
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
    [{R#?RECORD.name,R#?RECORD.cookie_str,R#?RECORD.deployment_spec,
      R#?RECORD.deployment_records}||R<-Z].

get_info(ClusterName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==ClusterName])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.name,R#?RECORD.cookie_str,
			    R#?RECORD.deployment_spec,
			    R#?RECORD.deployment_records}||R<-Z],
		   Info
	   end,
    Result.

get(Key,ClusterName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==ClusterName])),
    Result=case Z of
	       []->
		   {error,[eexist,ClusterName,?MODULE,?LINE]};
	       [R] ->
		   case  Key of
		       name->
			   {ok,R#?RECORD.name};
		       cookie_str->
			   {ok,R#?RECORD.cookie_str};
		       deployment_spec->
			   {ok,R#?RECORD.deployment_spec};
		       deployment_records->
			   {ok,R#?RECORD.deployment_records};
		       Err ->
			   {error,['Key eexists',Err,ClusterName,?MODULE,?LINE]}
		   end
	   end,
    Result.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.name||R<-Z].
    
set(Key,Value,ClusterName)->
  F = fun() -> 
	      Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
			       X#?RECORD.name==ClusterName])),
	      case Z of
		  []->
		      mnesia:abort({error,[eexist,ClusterName,?MODULE,?LINE]});
	       [R] ->
		      case Key of
			  deployment_records->
			      R1=R#?RECORD{deployment_records=Value},
			      mnesia:write(R1);
			  Reason ->
			      mnesia:abort({error,["Key eexists ",Reason,?MODULE,?LINE]})
		      end			   
	      end
      end,
    case mnesia:transaction(F) of
	{atomic, Val} ->
	    Val;
	{aborted,Reason}->
	    Reason
    end.
    
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
			{ok,[{cluster_spec,Info}]}->
			    {name,ClusterName}=lists:keyfind(name,1,Info),
			    {cookie_str,CookieStr}=lists:keyfind(cookie_str,1,Info),
			    {deployment_spec,DeploymentSpec}=lists:keyfind(deployment_spec,1,Info),
			    {deployment_records,DeploymentRecords}=lists:keyfind(deployment_records,1,Info),
			   case create(ClusterName,CookieStr,DeploymentSpec,DeploymentRecords) of
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
			   
