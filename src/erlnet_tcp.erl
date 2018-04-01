-module(erlnet_tcp).
-author("lkness").

-export([listen/1]).
-export([accept/2]).
-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([send/2]).
-export([sendfile/2]).
-export([sendfile/4]).
-export([sendfile/5]).
-export([setopts/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([shutdown/2]).
-export([close/1]).


-type tcp_sock_opts() :: [gen_tcp:listen_option()].
-export_type([tcp_sock_opts/0]).

-spec listen(tcp_sock_opts()) -> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
    %% listen(int sockfd, int backlog).  default:100
    Opts1 = sockopt_set_default(backlog, 100, Opts),
    %% Nagle algorithm. default: true
    Opts2 = sockopt_set_default(nodelay, true, Opts1),
    Opts3 = sockopt_set_default(send_timeout, 10000, Opts2),
    Opts4 = sockopt_set_default(send_timeout_close, true, Opts3),
    DisableSockOpts = [
        %% active once mode
        %% default block io
        active,
        %% default reuse address
        reuseaddr,
        %% default raw
        packet,
        header,
        mode,
        packet_size,
        line_delimiter
    ],
    MustDefaultSockOpts = [binary, {active, true}, {packet, raw}, {reuseaddr, true}],
    Opts5 = sockopt_del_disable(DisableSockOpts, Opts4),
    Opts6 = sockopt_add_default(MustDefaultSockOpts, Opts5),
    gen_tcp:listen(0, Opts6).

accept(SockFd, Timeout) ->
    gen_tcp:accept(SockFd, Timeout).

connect(Host, Port, Opts) when is_integer(Port) ->
    gen_tcp:connect(Host, Port, Opts ++ [binary, {active, false}, {packet, raw}]).

connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
    gen_tcp:connect(Host, Port, Opts ++ [binary, {active, false}, {packet, raw}], Timeout).

recv(SockFd, Length, Timeout) ->
    gen_tcp:recv(SockFd, Length, Timeout).

send(SockFd, Packet) ->
    gen_tcp:send(SockFd, Packet).

sendfile(SockFd, FileName) ->
    sendfile(SockFd, FileName, 0, 0, []).

sendfile(SockFd, File, Offset, Bytes) ->
    sendfile(SockFd, File, Offset, Bytes).

sendfile(SockFd, FileName, Offset, Bytes, Opts)
    when is_list(FileName) orelse is_atom(FileName)
    orelse is_binary(FileName) ->
    case file:open(FileName, [read, raw, binary]) of
        {ok, RawFile} ->
            try sendfile(SockFd, RawFile, Offset, Bytes, Opts) of
                Result -> Result
            after
                ok = file:close(RawFile)
            end ;
        {error, _} = Error -> Error
    end;
sendfile(SockFd, RawFile, Offset, Bytes, Opts) ->
    Opts2 =
        case Opts of
            [] -> [{chunk_size, 16#1fff}];
            _ -> Opts
        end ,
    try file:sendfile(RawFile, SockFd, Offset, Bytes, Opts2) of
        Result -> Result
    catch
        error:{badmatch, {error, enotconn}} ->
            {error, closed}
    end .

setopts(SockFd, Opts) ->
    inet:setopts(SockFd, Opts).

controlling_process(SockFd, Pid) ->
    gen_tcp:controlling_process(SockFd, Pid).

peername(SockFd) ->
    inet:peername(SockFd).

sockname(SockFd) ->
    inet:sockname(SockFd).

shutdown(SockFd, How) ->
    gen_tcp:shutdown(SockFd, How).

close(SockFd) ->
    gen_tcp:close(SockFd).





%%====================================================================
%% Internal functions
%%====================================================================

sockopt_set_default(Key, DefaultValue, Opts) ->
    case lists:keymember(Key, 1, Opts) of
        true -> Opts;
        false -> [{Key, DefaultValue} | Opts]
    end .

sockopt_del_disable(DefaultKeys, Opts) ->
    Fun = fun(Key, Acc) ->
        lists:keydelete(Key, 1, Acc)
        end,
    lists:foldl(Fun, Opts, DefaultKeys).

sockopt_add_default(DefaultKeys, Opts) ->
    Opts ++ DefaultKeys.