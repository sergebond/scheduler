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

-export([test/0]).

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
schedule(TaskID, PID, Type, {rel, Time}) ->
    schedule(TaskID, PID, Type, now() + Time); %% @todo Now() нужен правильный

schedule(TaskID, PID, Type, Time) ->
    gen_server:call(?SRV, {schedule, TaskID, PID, Type, Time}). %% @todo нужен call или cast?

-spec unschedule(TaskID :: binary(), PID :: integer()) -> ok.
unschedule(TaskID, PID) ->
    gen_server:cast(?SRV, {unschedule, TaskID, PID}). %% @todo нужен call или cast?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   TEST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    schedule(123456, 10, static, now()),
    timer:sleep(1000),
    schedule(122323, 20, static, now()),
    schedule(1233456, 30, dynamic, now()),
    schedule(1263456, 40, static, now()),
    schedule(1723456, 50, static, now()),
    schedule(1234586, 60, static, now()).

