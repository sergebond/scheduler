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

-include("scheduler.hrl").

-define(HEARTBEAT_INTERVAL, 5000).
-record(task, {
    taskID :: integer(),
    time :: integer(),
    type :: staic|dynamic}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({TaskID, PID, Type, Time} = Args) ->
    io:format("~nQueues srv started with params ~p", [Args]),
    self() ! heartbeat,
    {ok, {PID, [#task{taskID = TaskID, time = Time, type = Type}]}}.

handle_call({put, TaskID, Type, Time}, _From, State) ->
    Task = #task{taskID = TaskID, time = Time, type = Type},
    NewState = lists:keystore(TaskID, #task.taskID, Task, State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(heartbeat, State) ->
    do_job(State),
    %% @todo можно заморочиться с timer  https://habrahabr.ru/post/155173/
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_job(State) ->
    io:format("~nQueueHeartbeat PID ~p", [self()]),
    spawn_link(
        fun() ->
            {UpdatedSortedTasks, Length } = maybe_apply_tasks(State),
            UpdatedSortedTasks = maybe_store_part_to_db(UpdatedSortedTasks, Length)
        end).

maybe_apply_tasks(Tasks) ->
    SortedTasks = lists:keysort(#task.time, Tasks),
    Time = current_time(),
    TasksToApply = lists:takewhile(fun(Task) -> Task#task.time =:= Time end, SortedTasks),
    OldLength = erlang:length(TasksToApply),
    case OldLength of
        0 ->
            {SortedTasks, OldLength};
        NotNullLength ->
            gen_server:call(scheduler_srv, {process, TasksToApply}), %% @todo доделать
            {lists:nthtail(NotNullLength, SortedTasks), OldLength - NotNullLength}
    end.

maybe_store_part_to_db(SortedTasks, Length) ->
    case Length - ?QUEUE_LENGTH of
        0 -> refill_from_db();
        Norm when Norm < 0 -> SortedTasks;
        TooLong ->
            cut_to_db()
    end.

push_to_mq(TaskID, Type) ->
    io:format("Push to mq TaskID ~p, Type ~p", [TaskID, Type]).

current_time() ->
    erlang:system_time(1).

refill_from_db() ->
    ok.

cut_to_db() ->
    ok.