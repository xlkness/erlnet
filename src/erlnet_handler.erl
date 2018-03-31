-module(erlnet_handler).
-author("lkness").

-behaviour(gen_server).

-export([start_link/1]).
-export([handle_client/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(handler_state, {
    index = 0
}).

handle_client(HandlerPid, ClientSockFd, ClientCbMod) ->
    gen_server:call(HandlerPid, {handle_client, ClientSockFd, ClientCbMod}).

start_link(N) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [N], []).

init([N]) ->
    io:format("start [~w] handler ok.~n", [N]),
    {ok, #handler_state{index = N}}.

handle_call({handle_client, ClientSockFd, ClientCbMod}, _, State) ->
    StartClientRet =
        try ClientCbMod:start_link(ClientSockFd) of
            {ok, _Pid} -> ok;
            Ret ->
                io:format("start client cb(~p) process return error:~p~n",
                    [erlnet_tcp:peername(ClientSockFd), Ret]),
                erlnet_tcp:close(ClientSockFd),
                {error, Ret}
        catch
            E:R ->
                io:format("start client cb(~p) process return error:~p~n",
                    [erlnet_tcp:peername(ClientSockFd), {E, R}]),
                erlnet_tcp:close(ClientSockFd),
                {error, E, R}
        end,
    {reply, StartClientRet, State};
handle_call(_, _, State) -> {reply, unsupported_msg, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

terminate(_, _) -> ok.

code_change(_, State, _) -> {ok, State}.