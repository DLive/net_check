%%%-------------------------------------------------------------------
%% @doc net_check top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(net_check_sup).

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
                 intensity => 1000,
                 period => 5},
    ChildSpecs = [
        child_info(net_counter,net_counter,[]),
        child_info(net_client,net_client,[])
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
child_info(ID,Module,Args)->
    #{
        id => ID,
        start => {Module,start_link,Args}
    }.