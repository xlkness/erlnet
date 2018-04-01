%%%-------------------------------------------------------------------
%% @doc erlnet top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlnet_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([is_sup_alive/0]).
-export([start_handlers/3]).
-export([start_acceptors/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

is_sup_alive() ->
	Pid = erlang:whereis(?MODULE),
	is_pid(Pid) andalso erlang:is_process_alive(Pid).

start_handlers(SockType, ClientCbMod, ErlNetParam) ->
	ChildSpec = {erlnet_handlers_sup, {erlnet_handlers_sup, start_link, [SockType, ClientCbMod, ErlNetParam]},
				 permanent, 2000, supervisor, [erlnet_handlers_sup]},
	supervisor:start_child(?MODULE, ChildSpec).

start_acceptors(SockType, SockOpts, ClientCbMod, ErlNetParam) ->
	ChildSpec = {erlnet_acceptors_sup, {erlnet_acceptors_sup, start_link,
		[SockType, SockOpts, ClientCbMod, ErlNetParam]}, permanent, 2000, supervisor, [erlnet_acceptors_sup]},
	supervisor:start_child(?MODULE, ChildSpec).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 2, 10}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
