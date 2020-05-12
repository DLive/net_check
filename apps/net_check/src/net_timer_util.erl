%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2020 20:11
%%%-------------------------------------------------------------------
-module(net_timer_util).
-author("dlive").

%% API
-export([timestamp/0]).

timestamp()->
  {M, S, Micro} = os:timestamp(),
  M * 1000000 + S*1000 + Micro div 1000.