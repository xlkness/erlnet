%%%-------------------------------------------------------------------
%%% @author likun
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2018 3:16 PM
%%%-------------------------------------------------------------------
-module(tcp_connect_SUITE).
-author("likun").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [].

init_per_suite(Config) ->
	ct:print("===== start common test===="),
	Config.

end_per_suite(_) -> ok.

init_per_group(_, Config) -> Config.

end_per_group(_, _Config) -> ok.

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

one_tcp_connect(_) ->
	[].



