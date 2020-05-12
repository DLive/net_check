%%%-------------------------------------------------------------------
%% @doc net_check public API
%% @end
%%%-------------------------------------------------------------------

-module(net_check_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    start_server(),
    net_check_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

start_server()->
    {ok, _} = ranch:start_listener(tcp_echo,
        ranch_tcp, [
            {port, server_port()},
            {delay_send,false},
            {packet,2},
            {mode,binary}
        ],
        net_server_protocol, []
    ).

server_port()->
    case os:getenv("NET_SERVER_PORT") of
        false->
            4375;
        Port ->
            list_to_integer(Port)
    end.
