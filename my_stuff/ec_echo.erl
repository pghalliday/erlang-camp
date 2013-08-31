-module(ec_echo).
-export([start/0, stop/0, send/2]).

start() ->
	spawn(fun() ->
		register(?MODULE, self()),
		loop()
	end).

stop() ->
	?MODULE ! stop.

send(Node, Msg) ->
	{?MODULE, Node} ! {self(), Msg}.

loop() ->
	receive
		stop ->
			ok;
		{From, Msg} ->
			From ! {node(), Msg},
			loop()
	end.
