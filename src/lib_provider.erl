%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>

-module(lib_provider). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("db_provider_spec.hrl").

%% External exports
-export([create_table/0,create_table/2,add_node/2]).
-export([create/1,delete/1]).
-export([get_info/1,get_all/0,get/2,get_all_id/0]).
-export([do/1]).
-export([member/1]).
-export([git_clone_load/0]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}
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

create(ApplName)->
    Record=#?RECORD{
		    appl_name=ApplName,
		    vsn=undefined,
		    app=undefined,
		    erl_args=undefined,
		    git_path=undefined
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).


create(ApplName,Vsn,App,ErlArgs,GitPath)->
    Record=#?RECORD{
		    appl_name=ApplName,
		    vsn=Vsn,
		    app=App,
		    erl_args=ErlArgs,
		    git_path=GitPath
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete(Spec) ->
    F = fun() ->
                mnesia:delete({?TABLE,Spec})

        end,
    mnesia:transaction(F).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
member(ApplName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.appl_name==ApplName])),
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
    [{R#?RECORD.appl_name,R#?RECORD.vsn,
      R#?RECORD.app,R#?RECORD.erl_args,R#?RECORD.git_path}||R<-Z].

get_info(ApplName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.appl_name==ApplName])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.appl_name,R#?RECORD.vsn,
			    R#?RECORD.app,R#?RECORD.erl_args,
			    R#?RECORD.git_path}||R<-Z],
		   Info
	   end,
    Result.

get(Key,ApplName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.appl_name==ApplName])),
    Result=case Z of
	       []->
		   {error,[eexist,ApplName,?MODULE,?LINE]};
	       [R] ->
		   case  Key of
		       appl_name->
			   {ok,R#?RECORD.appl_name};
		       vsn->
			   {ok,R#?RECORD.vsn};
		       app->
			   {ok,R#?RECORD.app};
		       erl_args->
			   {ok,R#?RECORD.erl_args};
		       git_path->
			   {ok,R#?RECORD.git_path};
		       Err ->
			   {error,['Key eexists',Err,ApplName,?MODULE,?LINE]}
		   end
	   end,
    Result.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.appl_name||R<-Z].
    

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
			   file:del_dir_r(TempDirName),    
	%		   os:cmd("rm -rf "++TempDirName),	
			   {error,Reason};
		       LoadResult->
			   file:del_dir_r(TempDirName),    
			 %  os:cmd("rm -rf "++TempDirName),	
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
    GitDir=filename:join(TempDirName,?ProviderSpecDir),
    GitPath=?GitPathProviderSpecs,
    file:del_dir_r(GitDir),    
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
%% @spec
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
			{ok,[{provider_spec,Info}]}->
			    {appl_name,ApplName}=lists:keyfind(appl_name,1,Info),
			    {vsn,Vsn}=lists:keyfind(vsn,1,Info),
			    {app,App}=lists:keyfind(app,1,Info),
			    {erl_args,ErlArgs}=lists:keyfind(erl_args,1,Info),
			    {git_path,GitPath}=lists:keyfind(git_path,1,Info),
			   case create(ApplName,Vsn,App,ErlArgs,GitPath) of
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
			   
