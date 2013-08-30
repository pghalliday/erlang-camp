%%% @author Peter Halliday
%%% @copyright 2013 Peter Halliday

-module(ec_concurrent).
-export([bf_seq/1]).

%% @doc simultaneously prints a list backward and forward
-spec bf_seq(list()) -> ok.
bf_seq(List) ->
	spawn(fun() -> 
		print(List)
	end),
	spawn(fun() -> 
		print(lists:reverse(List))
	end),
	ok.

print(List) ->
	lists:map(fun(Item) -> 
		io:format("~p~n", [Item])
	end, List).
