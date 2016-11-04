-module(scheduler_srv).
-author("srg").
-behaviour(gen_server).
-include("scheduler.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(HEARTBEAT_INTERVAL, 1000).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    io:format("~nScheduler srv started"),
    {ok, dict:new()}.

handle_call({schedule, TaskID, PID, Type, Time}, _From, State) ->
    NewState =
        case dict:find(PID, State) of
            {ok, QueueID} ->
                gen_server:call(QueueID, {put, TaskID, Type, Time}),
                State;
            error ->
%%                io:format("~nStarting Worker for ~p ", [PID]),
                {ok, NewQueuePid} =
                    supervisor:start_child(queues_sup, [{TaskID, PID, Type, Time}]),
                link(NewQueuePid), %% @todo Доделать обработку при закрытии очереди
                dict:store(PID, NewQueuePid, State)
        end,
    {reply, {ok, dict:fetch_keys(NewState)}, NewState};

handle_call({unschedule, TaskID, PID}, _From, State) ->
    case dict:find(PID, State) of
        {ok, QueueID} ->
            gen_server:cast(QueueID, {put, TaskID});
        error ->
            {error, not_found}
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

