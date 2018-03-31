%%%-------------------------------------------------------------------
%%% @author lkness
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2018 2:51 PM
%%%-------------------------------------------------------------------
-module(erlnet_handler_sup).
-author("lkness").

-behaviour(supervisor).

-include("erlnet.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                       MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
          [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    ClientHandlerNameFun = fun(N) -> list_to_atom(lists:concat([erlnet_handler, "_", N])) end,
    AChild = [{ClientHandlerNameFun(N), {erlnet_handler, start_link, []},
              permanent, 2000, worker, [erlnet_handler]} || N <- lists:seq(1, ?CLIENT_HANDLER_NUM)],
    {ok, {{one_for_one, 10, 36}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
