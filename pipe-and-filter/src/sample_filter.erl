%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc Filter implementation example.
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
-module(sample_filter).
-behaviour(gen_filter).

-export([init/0, configure_previous/2, configure_next/2, handle/1]).

%%--------------------------------------------------------------------
%% @doc Function that initialises the filter. A filter process is
%%      usually waiting in a loop for the messages accepted by its
%%      configure_next/2, configure_previous/2, and handle/1 functions.
%% @end
%% --------------------------------------------------------------------
-spec init() -> ok.
init() ->
    loop().

%%--------------------------------------------------------------------
%% @doc Function that configures the previous stage of a filter.
%% @end
%% --------------------------------------------------------------------
-spec configure_previous(Filter :: pid(),
			 PreviousFilter :: pid() | none) -> ok.
configure_previous(Filter, PreviousFilter) ->
    Filter ! {set_prev, PreviousFilter},
    ok.
   
%%--------------------------------------------------------------------
%% @doc Function that configures the next stage of a filter.
%% @end
%% --------------------------------------------------------------------
-spec configure_next(Filter :: pid(),
		     NextFilter :: pid() | none) -> ok.
configure_next(Filter, NextFilter) ->
    Filter ! {set_next, NextFilter},
    ok.

%%--------------------------------------------------------------------
%% @doc Function that handles the data in this kind of filter.
%% @end
%% --------------------------------------------------------------------
-spec handle(Data :: term()) -> term().
handle(Data) ->
    io:format("[SAMPLE FILTER ~p] Handling ~p~n", [self(), Data]),
    Data.



% Internal loop
loop() ->
    receive
	{set_next, N} ->
	    loop({undef, N});
	{set_prev, P} ->
	    loop({P, undef})
    end.

loop({undef, N}) ->
    receive
	{set_prev, P} ->
	    loop(P, N)
    end;
loop({P, undef}) ->
    receive
	{set_next, N} ->
	    loop(P, N)
    end.

loop(none, none) -> % I am a lone P
    receive
	{handle, Data, Who} ->
	    HandledData = ?MODULE:handle(Data),
	    Who ! {pipe_output, HandledData},
	    loop(none, none);
	stop ->
	    true = unregister(pipeandfilter),
	    ok
    end;
loop(none, NextPid) when is_pid(NextPid) -> % I am pipeandfilter_head
    receive
	{handle, Data, Who} ->
	    HandledData = ?MODULE:handle(Data),
	    NextPid ! {handle, HandledData, self(), Who},
	    loop(none, NextPid);
	stop ->
	    NextPid ! stop,
	    true = unregister(pipeandfilter),
	    ok
    end;
loop(PrevPid, none) when is_pid(PrevPid) -> % I am pipeandfilter_tail
    receive
	{handle, Data, PrevPid, Who} ->
	    HandledData = ?MODULE:handle(Data),
	    Who ! {pipe_output, HandledData},
	    loop(PrevPid, none);
	stop ->
	    ok
    end;
loop(PrevPid, NextPid) when is_pid(PrevPid), is_pid(NextPid) -> % I am a P in the middle
    receive
	{handle, Data, PrevPid, Who} ->
	    HandledData = ?MODULE:handle(Data),
	    NextPid ! {handle, HandledData, self(), Who},
	    loop(PrevPid, NextPid);
	stop ->
	    NextPid ! stop,
	    ok
    end.

