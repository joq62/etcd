%%%-------------------------------------------------------------------
%% @doc etcd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(etcd_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
		  #{id=>etcd,
		    start=>{etcd,start_link,[]}},
		  #{id=>etcd_host,
		    start=>{etcd_host,start_link,[]}},
		  #{id=>etcd_application,
		    start=>{etcd_application,start_link,[]}},
		  #{id=>etcd_deployment,
		    start=>{etcd_deployment,start_link,[]}},
		  #{id=>etcd_lock,
		    start=>{etcd_lock,start_link,[]}},
		  #{id=>etcd_zigbee_device,
		    start=>{etcd_zigbee_device,start_link,[]}}
		  		
		 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
