%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc Pipe and filter unit tests.
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
-module(pipe_and_filter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CLEANUP_TIMEOUT, 100).

%%--------------------------------------------------------------------
%% @doc Test case for starting one pipe and three filters.
%% @end
%% --------------------------------------------------------------------
-spec start_one_pipe_three_filters() -> boolean().
start_one_pipe_three_filters() ->
    ?assertMatch(ok, pipe_and_filter:start(sample_pipe,
					   [sample_filter,
					    sample_filter,
					    sample_filter])).

%%--------------------------------------------------------------------
%% @doc Test case for starting one pipe and one filter.
%% @end
%% --------------------------------------------------------------------
-spec start_one_pipe_one_filter() -> boolean().
start_one_pipe_one_filter() ->
    ?assertMatch(ok, pipe_and_filter:start(sample_pipe,
					   [sample_filter])).

%%--------------------------------------------------------------------
%% @doc Test case for sending data through the pipe and the filters.
%% @end
%% --------------------------------------------------------------------
-spec data_through_pipe_and_filters() -> boolean().
data_through_pipe_and_filters() ->
    ?assertMatch("hola", pipe_and_filter:handle("hola")).

%%--------------------------------------------------------------------
%% @doc Test case for stopping the pipe and the filters.
%% @end
%% --------------------------------------------------------------------
-spec stop_pipe_and_filters() -> boolean().
stop_pipe_and_filters() ->
    Result = pipe_and_filter:stop(),
    timer:sleep(?CLEANUP_TIMEOUT),
    ?assertMatch(ok, Result).

%%--------------------------------------------------------------------
%% @doc Meta-test case for pipe and filter (trivial case: one filter).
%% @end
%% --------------------------------------------------------------------
-spec trivial_pipe_and_filter_test_() -> boolean().
trivial_pipe_and_filter_test_() ->
    {setup,
     fun start_one_pipe_one_filter/0,
     fun(_) -> stop_pipe_and_filters() end,
     fun(_) ->
	     {inorder,
	      [{"Unit test for architecture usage", fun data_through_pipe_and_filters/0}]}
     end}.

%%--------------------------------------------------------------------
%% @doc Meta-test case for pipe and filter (3 filters).
%% @end
%% --------------------------------------------------------------------
-spec pipe_and_filter_test_() -> boolean().
pipe_and_filter_test_() ->
    {setup,
     fun start_one_pipe_three_filters/0,
     fun(_) -> stop_pipe_and_filters() end,
     fun(_) ->
	     {inorder,
	      [{"Unit test for architecture usage", fun data_through_pipe_and_filters/0}]}
     end}.
