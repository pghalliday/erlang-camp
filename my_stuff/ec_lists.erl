%%% @author Peter Halliday
%%% @copyright 2013 Peter Halliday

-module(ec_lists).
-export([print_each/1, twomult/1, twomultmap/1, yourmap/2]).

%% @doc print the list
-spec print_each(list()) -> ok.
print_each(List) ->
	print_each(List, 1).

%% @doc print an item and recurse
-spec print_each(list(), number()) -> ok.
print_each([], _Count) ->
	ok;
print_each([H|T], Count) ->
	io:format("~p is ~p~n", [Count, H]),
	print_each(T, Count + 1).

%% @doc multiply the elements of a list by 2
-spec twomult(list()) -> list().
twomult(List) ->
	twomult(List, []).

twomult([], NewList) ->
	lists:reverse(NewList);
twomult([H|T], NewList) ->
	twomult(T, [H * 2 | NewList]).

twomultmap(List) ->
	lists:map(fun(N) -> N * 2 end, List).

%% @doc a map implementation
-spec yourmap(fun(), list()) -> list().
yourmap(Function, List) ->
	yourmap(Function, List, []).

yourmap(_Function, [], NewList) ->
	lists:reverse(NewList);
yourmap(Function, [H|T], NewList) ->
	yourmap(Function, T, [Function(H) | NewList]).
