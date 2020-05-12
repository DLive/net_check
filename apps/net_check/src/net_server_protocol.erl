%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2020 19:42
%%%-------------------------------------------------------------------
-module(net_server_protocol).
-author("dlive").

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("net_check.hrl").
%% API
-export([start_link/0,start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket,transport}).
-define(TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Ref,Socket, Transport, Opts) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref,Socket, Transport, Opts}])}.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init({Ref,Socket, Transport, _Opts = []}) ->
%%  {ok, Socket} = ranch:handshake(Ref),
  ok = Transport:setopts(Socket, [{active, once}, {packet, 2},{mode,binary}]),
  gen_server:enter_loop(?MODULE, [],#state{socket=Socket, transport=Transport}, ?TIMEOUT).
%%
%%init([]) ->
%%  {ok, #state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info({tcp, Socket, Data}, State = #state{socket=Socket, transport=Transport}) ->
  Transport:setopts(Socket, [{active, once}]),
  DataTerm = binary_to_term(Data),
  case check_net_state(DataTerm) of
    {send,ReData}->
      Transport:send(Socket,ReData);
    ok ->
      ok
  end,
  {noreply, State,?TIMEOUT};
handle_info({tcp_closed, _Socket}, State = #state{}) ->
  logger:error("tcp_closed"),
  {stop, normal, State};
handle_info({tcp_error, _, Reason}, State = #state{}) ->
  {noreply, State,?TIMEOUT};

handle_info(timeout, State = #state{}) ->
  logger:error("more than 5 sencond not receive heartbeat"),
  {noreply, State,?TIMEOUT};
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


check_net_state(#net_health_info{id=ID, send_time = SendTime} = NetInfo)->
  Now = net_timer_util:timestamp(),
  Cost = Now - SendTime,
  if
    Cost > 100 ->
      logger:error("[ReciveHeartbeat] id:~p cost:~p send:~p arrive:~p",[ID,Cost,SendTime,Now]);
    true ->
      logger:info("[ReciveHeartbeat] id:~p cost:~p send:~p arrive:~p",[ID,Cost,SendTime,Now])
  end,

  NetInfo2 = NetInfo#net_health_info{back_send_time = net_timer_util:timestamp()},
  {send,term_to_binary(NetInfo2)};

check_net_state(_NetInfo)->

  ok.