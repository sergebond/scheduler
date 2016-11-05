-module(scheduler).
-author("srg").

-include("scheduler.hrl").

-export([start/0, stop/0]).
-export([schedule/4, unschedule/2]).

-export([benchmark/0]).

-define(SRV, scheduler_srv).

%%--------------------------------------------------------------------
%% @doc start application
%%--------------------------------------------------------------------

start() ->
    ok = application:start(scheduler).

%%--------------------------------------------------------------------
%% @doc stop application
%%--------------------------------------------------------------------

stop() ->
    application:stop(scheduler).

-spec schedule(TaskID :: binary(), PID :: integer(),
    Type :: static|dynamic,
    Time :: {rel, integer()}|integer()) -> ok.
schedule(TaskID, PID, Type, {rel, Time}) when is_integer(Time) ->
    schedule(TaskID, PID, Type, current_time() + Time);

schedule(TaskID, PID, Type, Time) ->
    gen_server:call(?SRV, {schedule, TaskID, PID, Type, Time}). %% @todo нужен call или cast?

-spec unschedule(TaskID :: binary(), PID :: integer()) -> ok.
unschedule(TaskID, PID) ->
    gen_server:call(?SRV, {unschedule, TaskID, PID}). %% @todo нужен call или cast?

%%%===================================================================
%%% Internal functions
%%%===================================================================
current_time() ->
    erlang:system_time(1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%              TEST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_tasks(IdFrom, IdTo) ->
    Rand = fun(Int) -> rand:uniform(Int) end,
    RandTime = fun(RandInterval) -> current_time() + rand:uniform(RandInterval) end,
    RandFromList = fun(List) -> lists:nth(rand:uniform(length(List)), List) end,
    [#task{pid = Rand(20),
        taskID = TaskID, %% Есть вероятность, что таски будут повторяться, чтобы потестить замену существующей,  TaskID - для набора уникальных тасок
        time = RandTime(20),
        type = RandFromList([static, dynamic]) }|| TaskID <- lists:seq(IdFrom, IdTo)].

benchmark() ->
    Tasks = generate_tasks(1, 200000),
    TasksToErase = generate_tasks(1, 100000),
    Length = length(Tasks),
    Length1 = length(TasksToErase),
%%    io:format("~n GENERATED Tasks ~p", [length(Tasks)]),
    F = fun() ->
            lists:foreach(fun(#task{pid = PID, taskID = TaskID, time = Time, type = Type}) ->
            schedule(TaskID, PID, Type, Time) end, Tasks),
            io:format("~nSceduling ends")
        end,
    F1 = fun() ->
            lists:foreach(fun(#task{taskID = TaskID, pid = PID}) -> unschedule(TaskID, PID)  end, TasksToErase),
            io:format("~nUnsceduling ends")
        end,
    {Time, _Value} = timer:tc(fun() -> pmap([F,F1]) end) ,
        io:format("
        ~n=======================BENCHMARK RESULTS============================~n
        ~p Insertions/Deletions Time is ~p sec
        AVERAGE: ~p operations per sec
        ~n====================================================================~n",
        [Length + Length1, Time/1000000, round((Length + Length1)/(Time/1000000))]).

pmap(ListOfFun) ->
    Pids = lists:map(fun(Fun) -> spawn_link(Fun) end, ListOfFun),

    wait_for_ending(Pids).

wait_for_ending([]) -> ok;
wait_for_ending(Pids) ->
    process_flag(trap_exit, true),
    receive
        {'EXIT', Pid, normal} ->
            wait_for_ending(lists:delete(Pid, Pids));
        Message ->
            io:format("~nSome another message ~p", [Message]),
            wait_for_ending(Pids)
    end.