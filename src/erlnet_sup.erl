%%%-------------------------------------------------------------------
%% @doc erlnet top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlnet_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
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

start_acceptors(SockType, SockOpts, ClientCbMod, ErlNetParam) ->
	case is_sup_alive() of 
		true -> 
			ChildSpec = {erlnet_acceptors_sup, {erlnet_acceptors_sup, start_link, 
				[SockType, SockOpts, ClientCbMod, ErlNetParam]}, permanent, 2000, supervisor, [erlnet_acceptors_sup},
			supervisor:start_child(?MODULE, ChildSpec);
		false ->
			{error, ?MODULE, not_start}
	end.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
