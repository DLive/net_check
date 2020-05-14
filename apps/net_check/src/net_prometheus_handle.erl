%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. May 2020 17:47
%%%-------------------------------------------------------------------
-module(net_prometheus_handle).
-author("dlive").
-behaviour(cowboy_handler).

-export([
    init/2,
    terminate/3
]).

%% ===================================================================
%% cowboy_handler callbacks
%% ===================================================================

init(Req, _Opts) ->
    handle(Req).

terminate(_Reason, _Req, _State) ->
    ok.

%% ===================================================================
%% Private functions
%% ===================================================================

handle(Request) ->
    Method = cowboy_req:method(Request),
    Request1 = gen_response(Method, Request),
    {ok, Request1, undefined}.

gen_response(<<"GET">>, Request) ->
    Headers=[
        {<<"content-type">>,<<"text/plain">>}
    ],
    Registry0 = cowboy_req:binding(registry, Request, <<"default">>),
    case prometheus_registry:exists(Registry0) of
        false ->
            cowboy_req:reply(404, #{}, <<"Unknown Registry">>, Request);
        Registry ->
            Data = gen_prometheus_response(Registry),
            cowboy_req:reply(200,maps:from_list(Headers),Data,Request)
    end;
gen_response(_, Request) ->
    Request.

gen_prometheus_response(Registry)->
    prometheus_text_format:format().
