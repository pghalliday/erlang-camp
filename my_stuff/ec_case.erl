%%% @author Peter Halliday
%%% @copyright 2013 Peter Halliday

-module(ec_case).
-export([is_a_list/1]).

%% @doc checks if something is a list
-spec is_a_list(any()) -> boolean().
is_a_list(Thing) ->
	case Thing of
		[] ->
			true;
		[_H|_T] ->
			true;
		_ ->
			false
	end.
