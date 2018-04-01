%%%-------------------------------------------------------------------
%%% @author lkness
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2018 2:51 PM
%%%-------------------------------------------------------------------
-module(erlnet_handlers_sup).
-author("lkness").

-behaviour(supervisor).

-include("erlnet.hrl").

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(SockType, ClientCbMod, ErlNetParam) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [SockType, ClientCbMod, ErlNetParam]).

init([SockType, ClientCbMod, ErlNetParam]) ->
    Children = [{erlnet_handler:get_name(N), {erlnet_handler, start_link, [N, SockType, ClientCbMod, ErlNetParam]},
              permanent, 2000, worker, [erlnet_handler]} || N <- lists:seq(1, ?CLIENT_HANDLER_NUM)],
    {ok, {{one_for_one, 10, 36}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
