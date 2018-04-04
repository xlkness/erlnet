%%%-------------------------------------------------------------------
%%% @author likun
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2018 1:49 PM
%%%-------------------------------------------------------------------
-module(tcp_connect_tests).
-author("likun").

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
	{setup,
		fun start/0,
		fun stop/1,
		fun (_SetupData) ->
			[
				tcp_connect()
			]
		end
	}.

start() -> ok.

stop(_) -> ok.

tcp_connect() ->
	[?_assertEqual(1, 1)].