%%% @author Peter Halliday
%%% @doc Exports a function to sum a sequence
%%% @copyright 2013 Peter Halliday

-module(ec_recur).
-export([sum_seq/1]).

%% @doc sum the sequence
-spec sum_seq(number()) -> number() | error.
sum_seq(N) when N >= 0 ->
	sum_seq(N, 0);
sum_seq(_INVALID) ->
	error.

%% @doc sum the next sequence
-spec sum_seq(number(), number()) -> number().
sum_seq(0, Acc) ->
	Acc;
sum_seq(N, Acc) ->
	sum_seq(N - 1, Acc + N).
