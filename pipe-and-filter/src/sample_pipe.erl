%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc Pipe implementation example.
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%% @end
%%%-------------------------------------------------------------------
-module(sample_pipe).
-behaviour(gen_pipe).

-export([init/1]).

%%--------------------------------------------------------------------
%% @doc Function that initialises the pipe. The list of filters is
%%      a list of module names. These modules MUST BE implementations
%%      of the gen_filter behaviour.
%% @end
%% --------------------------------------------------------------------
-spec init(FilterList :: nonempty_list(tuple(module()))) -> ok.
init(FilterList) ->
    PidList = [ spawn(Filter, init, []) || Filter <- FilterList],
    true = register(pipeandfilter, hd(PidList)),
    ok = configure_next(PidList, FilterList, [], []),
    ok.

% Internal stuff
configure_next([P], [F], AccP, AccF) ->
    F:configure_next(P, none),
    configure_previous([P | AccP], [F | AccF]);
configure_next([P1, P2 | MorePs], [F1, F2 | MoreFs], AccP, AccF) ->
    F1:configure_next(P1, P2),
    configure_next([P2 | MorePs], [F2 | MoreFs], [P1 | AccP], [F1 | AccF]).

configure_previous([P], [F]) ->
    F:configure_previous(P, none);
configure_previous([P1, P2 | MorePs], [F1, F2 | MoreFs]) ->
    F1:configure_previous(P1, P2),
    configure_previous([P2 | MorePs], [F2 | MoreFs]).



