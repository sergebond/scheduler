%%%-------------------------------------------------------------------
%%% @author srg
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2016 17:39
%%%-------------------------------------------------------------------
-module(scheduler).
-author("srg").


-export([start/0, stop/0]).
-export([schedule/4, unschedule/2]).

-export([test/0, test1/0]).

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
    gen_server:cast(?SRV, {unschedule, TaskID, PID}). %% @todo нужен call или cast?

%%%===================================================================
%%% Internal functions
%%%===================================================================
current_time() ->
    erlang:system_time(1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   TEST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test1()->
    schedule(1234576, 10, static, current_time() + 10).

test() ->
    schedule(1234576, 10, static, current_time()),
    timer:sleep(1000),
    schedule(1224323, 20, static, current_time()),
    timer:sleep(1000),
    schedule(1233456, 30, dynamic, current_time()),
    timer:sleep(1000),
    schedule(1263456, 40, static, current_time()),
    timer:sleep(1000),
    schedule(1723456, 50, static, current_time()),
    timer:sleep(1000),
    schedule(1234586, 60, static, current_time()).