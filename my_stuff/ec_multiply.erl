%%% @author Peter Halliday
%%% @doc Exports a function to multiply 3 numbers
%%% @copyright 2013 Peter Halliday

-module(ec_multiply).
-export([multiply/3]).

%% @doc multiply 3 numbers
-spec(multiply(number(), number(), number()) -> number()).
multiply(A, B, C) ->
	A * B * C.
