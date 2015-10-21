%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc Pipe and filter coordination module.
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
-module(pipe_and_filter).

-export([start/2, handle/1, stop/0]).

%%--------------------------------------------------------------------
%% @doc Function that initialises the pipe and filter architecture.
%%      The pipe MUST BE an implementation of the gen_pipe behaviour,
%%      and the filters MUST BE implementations of the gen_filter
%%      behaviour.
%% @end
%% --------------------------------------------------------------------
-spec start(Pipe :: module(),
	    FilterConfig :: list({Filter :: module(), Args :: list(term())})) -> ok.
start(PipeModule, FilterList) ->
    PipeModule:init(FilterList).

%%--------------------------------------------------------------------
%% @doc Function that sends some Data through the filters in the pipe.
%% @end
%% --------------------------------------------------------------------
-spec handle(Data :: term()) -> HandledData :: term().
handle(Data) ->
    Pid = whereis(pipeandfilter),
    Pid ! {handle, Data, self()},
    receive
	{pipe_output, HandledData} ->
	    HandledData
    end.
					       
%%--------------------------------------------------------------------
%% @doc Function that stops the pipe and the filters.
%% @end
%% --------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    Pid = whereis(pipeandfilter),
    Pid ! stop,
    ok.
