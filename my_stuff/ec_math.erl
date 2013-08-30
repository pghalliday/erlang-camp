%%% @author Peter Halliday
%%% @doc Exports a function to apply math operators
%%% @copyright 2013 Peter Halliday

-module(ec_math).
-export([op/3]).

%% @doc apply an operation
-spec op(add | sub | any(), number(), number()) -> number() | error.
op(add, X, Y) ->
	X + Y;
op(sub, X, Y) when X >= Y ->
	X - Y;
op(sub, _X, _Y) ->
	'error: cannot subtract a big number from a smaller number';
op(_BAD_OPERATOR, _X, _Y) ->
	'error: unknown operator'.
