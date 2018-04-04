%%%-------------------------------------------------------------------
%%% @author likun
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2018 3:16 PM
%%%-------------------------------------------------------------------
-module(tcp_connect_tests).
-author("likun").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [].

init_per_suite(_Config) ->
	ct:print("===== start common test===="),
	config.

end_per_suite(_) -> ok.

init_per_group(_, Config) -> Config.

end_per_group(_, Config) -> Config.

init_per_testcase(_, Config) -> Config.

end_per_testcase(_, Config) -> Config.


groups() ->
	[
		{tcp_connect, [shuffle],
			[
				one_tcp_connect
			]}
	].

all() ->
	[
		{group, tcp_connect}
	].

one_tcp_connect() ->
	1 = 1,
	[].



