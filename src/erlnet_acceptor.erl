-module(erlnet_acceptor).
-author("lkness").

-include("erlnet.hrl").

-export([start_link/5]).

start_link(N, SockFd, SockType, ClientCbMod, ErlNetParam) ->
    Pid = spawn_link(fun() -> loop(N, SockFd, SockType, ClientCbMod, ErlNetParam) end),
    {ok, Pid}.

loop(N, SockFd, SockType, ClientCbMod, ErlNetParam) ->
    case SockType:accept(SockFd, infinity) of
        {ok, CSockFd} ->
            HandlerPid = erlnet_handler:get_pid(N rem ?CLIENT_HANDLER_NUM + 1),
            SockType:controlling_process(CSockFd, HandlerPid),
            erlnet_handler:handle_client(HandlerPid, CSockFd);
        {error, emfile} ->
            error_logger:warning_msg("max file discriptors num!!! check your ulimit.~n"),
            receive after 100 -> ok end;
        {error, closed} ->
            skip;
        {error, _Reason} ->
            ok
    end ,
    flush_mailbox(),
    loop(N, SockFd, SockType, ClientCbMod, ErlNetParam).

flush_mailbox() ->
    receive Any -> Any, flush_mailbox() after 0 -> ok end.