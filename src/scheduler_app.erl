-module(scheduler_app).

-behaviour(application).
-include("scheduler.hrl").

-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, SchedulerSup} = scheduler_sup:start_link(),
    {ok, _QueuesSup} =  supervisor:start_child(SchedulerSup, ?CHILD(queues_sup, supervisor)).

stop(_State) ->
    ok.
