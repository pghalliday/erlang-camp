%%% @author Peter Halliday
%%% @doc Exports a function to match to a
%%% @copyright 2013 Peter Halliday

-module(ec_match).
-export([a_is_a/1]).

%% @doc match to a
-spec a_is_a(a | any()) -> boolean().
a_is_a(a) ->
	true;
a_is_a(_X) ->
	false.
