-module(queues_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, child_spec/4]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("~nQueues sup started"),
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = transient,
    Shutdown = 1000,
    Type = worker,
    AChild = {'queue_srv', {'queue_srv', start_link, []},
        Restart, Shutdown, Type, []},
    {ok, {SupFlags, [AChild]}}.

child_spec(Id, Mod, Type, Args) ->
    {Id, {Mod, start_link, Args}, permanent, 500, Type, [Mod]}.