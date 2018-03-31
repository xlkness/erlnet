-module(erlnet).
-author("lkness").

-export([start_listen/4]).


start_listen(SockType, SockOpts, ClientCbMod, ErlNetParam) ->
	NewErlNetParam = 
		case maps:get(acceptor_num, ErlNetParam, undefined) of 
			undefined -> ErlNetParam#{acceptor_num => 10};
			_ -> ErlNetParam
		end,
	do_start_listen(SockType, SockOpts, ClientCbMod, NewErlNetParam).

do_start_listen(SockType, SockOpts, ClientCbMod, ErlNetParam) ->
	{ok, _} = erlnet_sup:start_acceptors(SockType, SockOpts, ClientCbMod, ErlNetParam).
