-module(erlnet_acceptor).
-author("lkness").

-export([start_link/5]).

start_link(N, SockFd, SockType, ClientCbMod, ErlNetParam) ->
    io:format("init [~w] acceptor ok, [~p/~p/~p]~n", [N, SockFd, ClientCbMod, ErlNetParam]),
    Pid = spawn_link(fun() -> loop(N, SockFd, SockType, ClientCbMod, ErlNetParam) end),
    {ok, Pid}.

loop(N, SockFd, SockType, ClientCbMod, ErlNetParam) ->
    case SockType:accept(SockFd, infinity) of
        {ok, CSockFd} ->
            ok;
        {error, emfile} ->
            io:format("max file discriptors num!!! check your ulimit.~n"),
            receive after 100 -> ok end;
        {error, Reason} ->
            io:format("acceptor [~w] return error:~p~n", [N, Reason])
    end ,
    loop(N, SockFd, SockType, ClientCbMod, ErlNetParam).