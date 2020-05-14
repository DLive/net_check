%%%-------------------------------------------------------------------
%% @doc net_check public API
%% @end
%%%-------------------------------------------------------------------

-module(net_check_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    start_server(),
    StartInfo = net_check_sup:start_link(),
    start_monitor(),
    StartInfo.

stop(_State) ->
    ok.

%% internal functions

start_server() ->
    {ok, _} = ranch:start_listener(tcp_echo,
        ranch_tcp, [
            {port, server_port()},
            {delay_send, false},
            {packet, 2},
            {mode, binary}
        ],
        net_server_protocol, []
    ).

server_port() ->
    case os:getenv("NET_SERVER_PORT") of
        false ->
            4375;
        Port ->
            list_to_integer(Port)
    end.

start_monitor() ->
    Routes = [
        {'_', [
            {"/metrics/[:registry]", net_prometheus_handle, []}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(monitor_http, [{port, 9153}],
        #{
            env => #{dispatch => Dispatch}
        }).