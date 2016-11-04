-module(aggregator_srv).
-author("srg").
-behaviour(gen_server).

%% API
-export([start_link/0]).

-define(HEARTBEAT_INTERVAL, 1000).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    self() ! heartbeat,
    {ok, []}.

handle_call({to_db, List}, _From, Tasks) when is_list(List) ->
    NewTasks =  Tasks ++ List,
    io:format("~n ++++++Length Tasks in aggregator ~p", [length(NewTasks)]),
    {reply, ok, NewTasks};

handle_call({from_db, PID, HowMuch}, _From, Tasks) ->
    save_to_db(Tasks),
    Tasks = get_from_db(PID, HowMuch),
    {reply, Tasks, []};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(heartbeat, Tasks) ->
    save_to_db(Tasks),
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    {noreply, []};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
save_to_db([]) ->
%%    io:format("Theres no tasks to save"),
ok;
save_to_db(Tasks) ->
    %% insert into tablename (id,blabla) values(1,'werwer'),(2,'wqewqe'),(3,'qwewe');
    io:format("SQL query for inserting group of tasks lenghth ~p", [length(Tasks)]).

get_from_db(PID, Quantity) ->
    %% SELECT FROM tablename WHERE pid = $PID ORDER BY time DESC LIMIT $quantity Примерный запрос в базу
    io:format("SQL query for selecting and deleting group of tasks PID ~p, quantity ~p", [PID, Quantity]),
    ok.