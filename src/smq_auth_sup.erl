%%%-------------------------------------------------------------------
%% @doc smq_auth top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(smq_auth_sup).

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
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },

    % Start your client worker
    ChildSpecs = [
        #{
            id => smq_auth_worker,
            start => {smq_auth_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [smq_auth_worker]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
