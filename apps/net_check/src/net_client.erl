%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(net_client).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(HEARTBEAT_TIME, 2000).

-record(state, {host, port, socket =undefined, heart_list = []}).
-include("net_check.hrl").
%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    case get_server_info() of
        error ->
            {stop, target_server_no_config};
        {Host, Port} ->
            erlang:send_after(1000, self(), reconnection),
            {ok, #state{host = Host, port = Port}}
    end.
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.
handle_info({tcp, Socket, Data}, State = #state{heart_list = HeartList}) ->
    inet:setopts(Socket, [{active, once}]),
    DataTerm = binary_to_term(Data),
    case check_net_state(DataTerm) of
        {ok, Index} ->
            HeartList2 = lists:delete(Index, HeartList),
            {noreply, State#state{heart_list = HeartList2}}
    end;
handle_info(heartbeat, State = #state{socket = undefined, heart_list = HeartList}) ->
    {noreply, State};
handle_info(heartbeat, State = #state{socket = Socket, heart_list = HeartList}) ->
    case checklist(HeartList) andalso do_heartbeat(Socket) of
        false ->
            gen_tcp:close(Socket),
            erlang:send_after(1000, self(), reconnection),
            {noreply, State#state{socket = undefined, heart_list = []}};
        {ok, HeartIndex} ->
            erlang:send_after(?HEARTBEAT_TIME, self(), heartbeat),
            {noreply, State#state{heart_list = [HeartIndex] ++ HeartList}};
        {error, Reason} ->
            logger:error("send heartbeat error Reason ~p, will close and reopen", [Reason],#{domain => net_client}),
            print_lost_heartbeat(HeartList),
            gen_tcp:close(Socket),
            erlang:send_after(1000, self(), reconnection),
            {noreply, State#state{socket = undefined, heart_list = []}}
    end;
handle_info(reconnection, State = #state{host = Host, port = Port}) ->
    case connect_server(Host, Port) of
        {ok, Socket} ->
            {noreply, State#state{socket = Socket}};
        {error, _Reason} ->
            erlang:send_after(2000, self(), reconnection),
            {noreply, State#state{socket = undefined}}
    end;
handle_info({tcp_closed,_}, State = #state{}) ->
    erlang:send_after(1000, self(), reconnection),
    {noreply, State#state{socket = undefined}};
handle_info(Info, State = #state{}) ->
    logger:error("client get unkonw handle message ~p",[Info],#{domain => net_client}),
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect_server(Host, Port) ->
    Options = [
        {active,once},
        {delay_send,false},
        {packet,2},
        {mode,binary},
        {send_timeout,2000}
    ],
    try gen_tcp:connect(Host, Port, Options, 2000) of
        {ok, Socket} ->
            logger:info("connection to server success",#{domain => net_client}),
            erlang:send_after(?HEARTBEAT_TIME, self(), heartbeat),
            {ok, Socket};
        UnKnow ->
            logger:error("connect to server ~p:~p return msg ~p", [Host, Port, UnKnow],#{domain => net_client}),
            {error, UnKnow}
    catch
        _Type:Reason:_ ->
            logger:error("connect to server ~p:~p error ~p", [Host, Port, Reason],#{domain => net_client}),
            {error, Reason}
    end.

do_heartbeat(Socket) ->
    Index = net_counter:gen(),
    SendTime = net_timer_util:timestamp(),
    HeartBeat = term_to_binary(#net_health_info{id = Index, send_time = SendTime}),
    case gen_tcp:send(Socket, HeartBeat) of
        ok ->
            logger:info("[heartbeat] id:~p send_time:~p", [Index, SendTime],#{domain => net_client}),
            {ok, Index};
        {error, Reason} ->
            {error, Reason}
    end.
print_lost_heartbeat([]) ->
    ok;
print_lost_heartbeat([Item | HeartList]) ->
    logger:error("[heartbeat_back_lost] ~p", [Item],#{domain => net_client}),
    print_lost_heartbeat(HeartList).

check_net_state(#net_health_info{id = ID, send_time = SendTime, back_send_time = BackSendTime} = NetInfo) ->
    Now = net_timer_util:timestamp(),
    Cost = Now - SendTime,
    BackCost = Now - BackSendTime,
    if
        Cost > 15 ->
            logger:error("[HeartbeatBack] id:~p cost:~p backcost:~p send:~p arrive:~p", [ID, Cost, BackCost, SendTime, Now],#{domain => net_client});
        true ->
            logger:info("[HeartbeatBack] id:~p cost:~p backcost:~p send:~p arrive:~p", [ID, Cost, BackCost, SendTime, Now],#{domain => net_client})
    end,
    {ok, ID};

check_net_state(NetInfo) ->
    logger:error("check_net_state unkonw message ~p", [NetInfo],#{domain => net_client}),
    ok.

checklist(HeartList) ->
    case length(HeartList) of
        Len when Len > 15 ->
            logger:error("more then 15 heartbeat no back ~p", [HeartList],#{domain => net_client}),
            false;
        _ ->
            true
    end.


get_server_info() ->
    case os:getenv("NET_CHECK_HOST") of
        false ->
            error;
        HostPort ->
            [Host, Port] = string:split(HostPort, ":"),
            {Host, list_to_integer(Port)}
    end.