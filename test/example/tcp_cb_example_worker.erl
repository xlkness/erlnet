%%%-------------------------------------------------------------------
%%% @author lkness
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2018 10:35 AM
%%%-------------------------------------------------------------------
-module(tcp_cb_example_worker).
-author("lkness").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(client_state, {
    sockfd
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SockFd) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [{SockFd}]),
    {ok, Pid}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({SockFd}) when is_port(SockFd) orelse is_pid(SockFd) ->
%%    proc_lib:init_ack(Parent, init_ok),
%%    {ok, {Addr, Port}} = erlnet_tcp:peername(SockFd),
%%    io:format("create one client sock process, peername:~p:~p~n", [Addr, Port]),
    erlnet_tcp:setopts(SockFd, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #client_state{sockfd = SockFd});
init(_) ->
    io:format("create one client sock process, peername:~p~n", [test]),
    gen_server:enter_loop(?MODULE, [], #client_state{sockfd = test}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) when byte_size(Data) >= 1 ->
%%    io:format("receive [~p] client msg:~p~n", [erlnet_tcp:peername(Socket), Data]),
%%    erlnet_tcp:send(Socket, Data),
    erlnet_tcp:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, {tcp_receive_error, Reason}, State};
handle_info(_, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
