%%%-------------------------------------------------------------------
%%% @author lkness
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2018 10:34 AM
%%%-------------------------------------------------------------------
-module(tcp_cb_example_sup).
-author("lkness").

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_link/1]).
-export([start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_link(SockFd) ->
    start_child(SockFd).

start_child(Args) ->
    supervisor:start_child(?MODULE, [Args]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    AChild = {tcp_cb_example_worker, {tcp_cb_example_worker, start_link, []},
              transient, 2000, worker, [tcp_cb_example_worker]},

    {ok, {{simple_one_for_one, 1000, 3600}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
