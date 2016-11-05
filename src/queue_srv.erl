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

-export([maybe_apply_tasks/1,
    maybe_store_part_to_db/2]). %% For tests

-include("scheduler.hrl").

-define(HEARTBEAT_INTERVAL, 1000).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({TaskID, PID, Type, Time} = Args) ->
    io:format("~nQueues srv started with params ~p", [Args]),
    self() ! heartbeat,
    State = {PID, [#task{taskID = TaskID, time = Time, type = Type}]},
    {ok, State}.

handle_call({put, TaskID, Type, Time}, _From, {Pid, Tasks}) ->
    Task = #task{taskID = TaskID, time = Time, type = Type},
    NewTasks = lists:keystore(TaskID, #task.taskID, Tasks, Task),
    {reply, ok, {Pid, NewTasks}};

handle_call({erase, TaskID}, _From, {Pid, Tasks}) ->
    NewTasks =
        case lists:keydelete(TaskID, #task.taskID, Tasks) of
            Tasks ->
                search_and_del_from_db(TaskID),
                Tasks;
            UpdatedTasks -> UpdatedTasks
        end,
    {reply, ok, {Pid, NewTasks}}.

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

do_job({PID, Tasks}) ->
    {UpdatedSortedTasks, Length } = maybe_apply_tasks(Tasks),
    Updated =  maybe_store_part_to_db(UpdatedSortedTasks, Length),
    {PID, Updated};

do_job(Other) ->
    io:format("~nEmpty queue ~p", [Other]).

-spec maybe_apply_tasks(Tasks :: list(#task{})) -> {UpdTasks :: list(#task{}), Length :: integer()}.
maybe_apply_tasks(Tasks) ->
    SortedTasks = lists:keysort(#task.time, Tasks),
    Time = current_time(),
    TasksToApply = lists:takewhile(fun(Task) -> Task#task.time =< Time  end, SortedTasks),
    OldLength = length(Tasks),
    ToApplyLength = erlang:length(TasksToApply),
    case ToApplyLength of
        0 ->
            {SortedTasks, OldLength};
        NotNullLength ->
            push_to_mq(TasksToApply),
            {lists:nthtail(NotNullLength, SortedTasks), OldLength - NotNullLength}
    end.

-spec maybe_store_part_to_db(list(#task{}), Lenghth :: non_neg_integer()) -> UpdTasks :: list(#task{}).
maybe_store_part_to_db(SortedTasks, Length) ->
    case Length - ?QUEUE_LENGTH of
        0 ->
            refill_from_db(SortedTasks, Length);
        Norm when Norm < 0 -> SortedTasks;
        _TooLong ->
            cut_to_db(SortedTasks, Length)
    end.

-spec push_to_mq(list(#task{})) -> ok.
push_to_mq(_Tasks) -> ok.
%%    gen_server:call(scheduler_srv, {process, Tasks}). %% @todo доделать.

-spec refill_from_db(list(#task{}), non_neg_integer()) -> list(#task{}).
refill_from_db(SortedTask, _Length) ->
    SortedTask.

-spec cut_to_db(list(#task{}), non_neg_integer()) -> list(#task{}).
cut_to_db(SortedTasks, _Length) ->
    ToBase = lists:nthtail(?QUEUE_LENGTH, SortedTasks),
    Remain = lists:sublist(SortedTasks, ?QUEUE_LENGTH),
    gen_server:call(aggregator_srv, {to_db, ToBase}),
    Remain.

search_and_del_from_db(_TaskID) ->
    ok. %% @todo доделать.

current_time() ->
    erlang:system_time(1).
