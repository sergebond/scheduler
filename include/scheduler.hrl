%%%-------------------------------------------------------------------
%%% @author srg
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Nov 2016 10:44
%%%-------------------------------------------------------------------
-author("srg").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(QUEUE_LENGTH, 1000).

-record(task, {
    pid :: integer(),
    taskID :: integer(),
    time :: integer(),
    type :: staic|dynamic}).