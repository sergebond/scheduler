-module(scheduler_sup).
-behaviour(supervisor).

-include("scheduler.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10},[
        ?CHILD(scheduler_srv, worker),
        ?CHILD(aggregator_srv, worker)
    ]}}.