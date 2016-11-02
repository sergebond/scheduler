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
-define(SRV, scheduler_srv).

-export([start/0, stop/0]).

%% API
-export([schedule/4,
    unschedule/2]).

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

-spec schedule(TaskID :: binary(),
    PID :: integer(),
    Type :: binary(),
    {rel, integer()}|integer()) -> ok.
schedule(TaskID, PID, Type, {rel, Time}) ->
    schedule(TaskID, PID, Type, now() + Time); %% @todo Now() нужен правильный

schedule(TaskID, PID, Type, Time) ->
    gen_server:cast(?SRV, {schedule, TaskID, PID, Type, Time}). %% @todo нужен call или cast?

-spec unschedule(TaskID :: binary(), PID :: integer()) -> ok.
unschedule(TaskID, PID) ->
    gen_server:cast(?SRV, {unschedule, TaskID, PID}). %% @todo нужен call или cast?