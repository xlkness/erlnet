-module(erlnet_acceptors_sup).
-author("lkness").

-export([start_link/4]).
-export([init/1]).

start_link(SockType, SockOpts, ClientCbMod, ErlNetParam) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [SockType, SockOpts, ClientCbMod, ErlNetParam]).

init([SockType, SockOpts, ClientCbMod, ErlNetParam]) ->
    #{acceptor_num := AcceptorNum} = ErlNetParam,
    case SockType:listen(SockOpts) of
        {ok, SockFd} ->
            ChildNameFun = fun(N) -> list_to_atom(lists:concat([erlnet_acceptor, "_", N])) end,
            Childs = [{ChildNameFun(N),
                       {erlnet_acceptor, start_link, [N, SockFd, SockType, ClientCbMod, ErlNetParam]},
                          permanent, 2000, worker, [erlnet_acceptor]} || N <- lists:seq(1, AcceptorNum)],
            {ok, {{one_for_one, 10, 36}, Childs}};
        {error, Reason} ->
            {error, {connot_create_sockfd, Reason}}
    end .


