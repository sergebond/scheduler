-module(queue_srv).
-author("srg").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).


-export([test/0]).

-include("scheduler.hrl").

-define(HEARTBEAT_INTERVAL, 5000).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({TaskID, PID, Type, Time} = Args) ->
    io:format("~nQueues srv started with params ~p", [Args]),
    self() ! heartbeat,
    State = {PID, [#task{taskID = TaskID, time = Time, type = Type}]},
    io:format("~n State is ~p", [State]),
    {ok, State}.

handle_call({put, TaskID, Type, Time}, _From, {Pid, Tasks}) ->
    Task = #task{taskID = TaskID, time = Time, type = Type},
    NewState = lists:keystore(TaskID, #task.taskID, Task, Tasks),
    {reply, ok, {Pid, NewState}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(heartbeat, State) ->

    NewState = do_job(State),
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_job({_PID, Tasks}) ->
    io:format("~nQueueHeartbeat PID ~p", [self()]),
            {UpdatedSortedTasks, Length } = maybe_apply_tasks(Tasks),
            UpdatedSortedTasks = maybe_store_part_to_db(UpdatedSortedTasks, Length);
do_job(Other) ->
    io:format("~nEmpty queue ~p", [Other]).


-spec maybe_apply_tasks(Tasks :: list(#task{})) -> {UpdTasks :: list(#task{}), Length :: integer()}.
maybe_apply_tasks(Tasks) ->
    SortedTasks = lists:keysort(#task.time, Tasks),
    Time = current_time(),
    TasksToApply = lists:takewhile(fun(Task) -> Task#task.time =< Time  end, SortedTasks),
%%    io:format("Task to apply ~p", [TasksToApply]),
    OldLength = length(Tasks),
    ToApplyLength = erlang:length(TasksToApply),
    io:format("~n ~p Tasks to run", [ToApplyLength]),
    case ToApplyLength of
        0 ->
            {SortedTasks, ToApplyLength};
        NotNullLength ->
            push_to_mq(TasksToApply), %% PID
            {lists:nthtail(NotNullLength, SortedTasks), OldLength - NotNullLength}
    end.

-spec maybe_store_part_to_db(list(#task{}), Lenghth :: non_neg_integer()) -> {UpdTasks :: list(#task{}), Length :: integer()}.
maybe_store_part_to_db(SortedTasks, Length) ->
    io:format("~n ***** length ~p", [Length]),
    case Length - ?QUEUE_LENGTH of
        0 ->
            refill_from_db(SortedTasks, Length);
        Norm when Norm < 0 -> SortedTasks;
        _TooLong ->
            cut_to_db(SortedTasks, Length)
    end.

-spec push_to_mq(list(#task{})) -> ok.
push_to_mq(Tasks) ->
    io:format("~nPushing to mq Tasks ~p", [length(Tasks)]),
    gen_server:call(scheduler_srv, {process, Tasks}). %% @todo доделать.

-spec refill_from_db(list(#task{}), non_neg_integer()) -> list(#task{}).
refill_from_db(SortedTask, _Length) ->
    SortedTask.

-spec cut_to_db(list(#task{}), non_neg_integer()) -> list(#task{}).
cut_to_db(SortedTasks, Length) ->

    ToBase = lists:nthtail(Length - ?QUEUE_LENGTH, SortedTasks),
    io:format("~n Tobase ~p tasks", [length(ToBase)]),
    Remain = lists:sublist(SortedTasks, ?QUEUE_LENGTH),
    io:format("~n Remainong ~p tasks", [length(Remain)]),
    gen_server:call(aggregator_srv, {to_db, ToBase}),
    Remain.

current_time() ->
    erlang:system_time(1).

test() ->
    Rand = fun(Int) -> rand:uniform(Int) end,
    RandTime = fun(RandInterval) -> current_time() + rand:uniform(RandInterval) end,
    RandFromList = fun(List) -> lists:nth(rand:uniform(length(List)), List) end,
    Tasks = [#task{pid = Rand(100), taskID = TaskID, time = RandTime(3), type = RandFromList([static, dynamic]) }|| TaskID <- lists:seq(1000, 5100)],
    timer:sleep(1000),
    io:format("~n!!!GENERATED ~p", [length(Tasks)]),
    {UpdatedSortedTasks, Length} = maybe_apply_tasks(Tasks),

    Res = maybe_store_part_to_db(UpdatedSortedTasks, Length),
%%    io:format("Tasks ~p", [Res]).
    io:format("~n !!! Remaining Lenghth of tasks ~p", [length(Res)]).