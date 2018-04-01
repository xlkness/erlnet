-module(erlnet_handler).
-author("lkness").

-behaviour(gen_server).

-export([get_name/1]).
-export([get_pid/1]).
-export([handle_client/2]).
-export([start_link/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(handler_state, {
    index = 0,
    clients_num = 0,
    sock_type,
    client_cb_mod,
    erl_net_param
}).

get_name(N) ->
    list_to_atom(lists:concat([erlnet_handler, "_", N])).

get_pid(N) ->
    erlang:whereis(get_name(N)).

handle_client(HandlerPid, ClientSockFd) ->
    gen_server:call(HandlerPid, {handle_client, ClientSockFd}).

start_link(N, SockType, ClientCbMod, ErlNetParam) ->
    gen_server:start_link({local, get_name(N)}, ?MODULE, [N, SockType, ClientCbMod, ErlNetParam], []).

init([N, SockType, ClientCbMod, ErlNetParam]) ->
    State = #handler_state{
        index = N, sock_type = SockType, clients_num = 0,
        client_cb_mod = ClientCbMod, erl_net_param = ErlNetParam},
    {ok, State}.

handle_call({handle_client, ClientSockFd}, _, State) ->
    #handler_state{
        clients_num = ClientNum, client_cb_mod = ClientCbMod,
        sock_type = SockType
    } = State,
    {StartClientRet, NewState} =
        try ClientCbMod:start_link(ClientSockFd) of
            {ok, Pid} ->
                ok = SockType:controlling_process(ClientSockFd, Pid),
                {ok, State#handler_state{clients_num = ClientNum + 1}};
            Ret ->
                error_logger:warning_msg("start client cb(~p) process return error:~p~n",
                    [SockType:peername(ClientSockFd), Ret]),
                SockType:close(ClientSockFd),
                {{error, Ret}, State}
        catch
            E:R ->
                error_logger:warning_msg("start client cb(~p) process crash error:~p~n",
                    [SockType:peername(ClientSockFd), {E, R}]),
                SockType:close(ClientSockFd),
                {{error, E, R}, State}
        end,
    {reply, StartClientRet, NewState};
handle_call(_, _, State) -> {reply, unsupported_msg, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

terminate(_, _) -> ok.

code_change(_, State, _) -> {ok, State}.