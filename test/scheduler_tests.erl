%%%-------------------------------------------------------------------
%%% @author srg
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Nov 2016 11:23
%%%-------------------------------------------------------------------
-module(scheduler_tests).
-author("srg").


%% API
-export([queue_apply_task_test_/0]).
-include_lib("eunit/include/eunit.hrl").
-include("scheduler.hrl").

queue_apply_task_test_() ->
    TasksAllToApply = generate_tasks(1000, 1011, 0),
    TasksNoneToApply = generate_tasks(341000, 341009, 100),
    {_, Length1} = queue_srv:maybe_apply_tasks(TasksNoneToApply),
%%    io:format("~p", [T]),
    {_, Length2} = queue_srv:maybe_apply_tasks(TasksAllToApply ++ TasksNoneToApply),
%%    io:format("~p", [{Length1, Length2}]),
    Length1 = Length2,
    fun () ->
        ?assertMatch({[], 0}, queue_srv:maybe_apply_tasks(TasksAllToApply)),
        ?assert(Length1 =:= 10),
        ?assert(Length2 =:= 10)
    end.

%%store_part_to_db_test() ->
%%    application:start(scheduler),
%%    Tasks = generate_tasks(1000, 3500, 0),
%%    Length1 = length(Tasks),
%%    Tasks1 = queue_srv:maybe_store_part_to_db(Tasks, length(Tasks)),
%%    Length2 =  length(Tasks1),
%%%%    application:stop(scheduler),
%%    fun () ->
%%        ?assert((Length1 - Length2) =:= 1000)
%%    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      INTERNALL
%%%%%%%%%%%%%%%%%%%%%%%%%%
current_time() ->
    erlang:system_time(1).


generate_tasks(IdFrom, IdTo, Time) ->
    Rand = fun(Int) -> rand:uniform(Int) end,
    RandTime = fun(Time) -> current_time() + Time end,
    RandFromList = fun(List) -> lists:nth(rand:uniform(length(List)), List) end,
    [#task{pid = Rand(20),
        taskID = TaskID, %% Есть вероятность, что таски будут повторяться, чтобы потестить замену существующей,  TaskID - для набора уникальных тасок
        time = RandTime(Time),
        type = RandFromList([static, dynamic]) }|| TaskID <- lists:seq(IdFrom, IdTo)].