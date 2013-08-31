-module(ttt_server).

-behaviour(gen_server).

%% API
-export([start_link/0, player/1, players/0, game/2, games/0, join/3, subscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(row, {cell1, cell2, cell3}).
-record(grid, {row1, row2, row3}).
-record(game, {id, player1, player2, grid, result}).
-record(player, {id, name, subscribers}).
-record(state, {players, next_player_id, games, next_game_id}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% register a player
-spec player(Name :: string()) -> number().
player(Name) ->
    gen_server:call(?SERVER, {player, Name}).

%% @doc
%% get the list of players
-spec players() -> list().
players() ->
    gen_server:call(?SERVER, {players}).

%% @doc
%% start a new game
-spec game(PlayerId :: number(), FirstMove :: tuple()) -> number() | error_no_player.
game(PlayerId, {X, Y}) when is_integer(X), is_integer(Y), X > 0, X < 4, Y > 0, Y < 4 ->
    gen_server:call(?SERVER, {game, PlayerId, {X, Y}}).

%% @doc
%% get the list of games
-spec games() -> list().
games() ->
    gen_server:call(?SERVER, {games}).

%% @doc
%% get the list of games
-spec join(PlayerId :: number(), GameId :: number(), FirstMove :: tuple()) -> ok | error_no_game | error_no_player | error_game_in_progress | error_cannot_play_yourself | error_invalid_move.
join(PlayerId, GameId, {X, Y}) when is_integer(X), is_integer(Y), X > 0, X < 4, Y > 0, Y < 4 ->
    gen_server:call(?SERVER, {join, PlayerId, GameId, {X, Y}}).

%% @doc
%% follow someone
%% What's the node stuff for - something to do with workers??
-spec subscribe(PlayerId, Node) -> ok when
      Node :: node(),
      PlayerId :: number().
subscribe(PlayerId, Node) ->
    gen_server:cast({?SERVER, Node}, {subscribe, {PlayerId, self()}}).

subscribe(PlayerId) ->
    subscribe(PlayerId, node()).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
init([]) ->
    {ok, #state{players = [], next_player_id = 0, games = [], next_game_id = 0}}.

%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
handle_call({player, Name}, _From, State) ->
	Id = State#state.next_player_id,
    {reply, Id, State#state{players = insert_player(Id, Name, State#state.players), next_player_id = Id + 1}};
handle_call({players}, _From, State) ->
    {reply, State#state.players, State};
handle_call({game, PlayerId, {X, Y}}, _From, State) ->
	case lists:keyfind(PlayerId, 2, State#state.players) of
		false ->
			{reply, error_no_player, State};
		_Player ->
			Id = State#state.next_game_id,
		    {reply, Id, State#state{games = insert_game(Id, PlayerId, {X, Y}, State#state.games), next_game_id = Id + 1}}
	end;
handle_call({games}, _From, State) ->
    {reply, State#state.games, State};
handle_call({join, PlayerId, GameId, {X, Y}}, _From, State) ->
	case lists:keyfind(PlayerId, 2, State#state.players) of
		false ->
			{reply, error_no_player, State};
		_Player ->
			case lists:keyfind(GameId, 2, State#state.games) of
				false ->
					{reply, error_no_game, State};
				Game ->
					if 
						Game#game.player2 /= undefined ->
							{reply, error_game_in_progress, State};
						true ->
							if 
								Game#game.player1 == PlayerId ->
									{reply, error_cannot_play_yourself, State};
								true ->
									case move_is_valid(Game#game.grid, {X, Y}) of
										false ->
											{reply, error_invalid_move, State};
										true ->
											NewGame = join_game(PlayerId, {X, Y}, Game),
											% notify player1's subscribers that it's their turn
											your_move(lists:keyfind(Game#game.player1, 2, State#state.players), NewGame),
								    		{reply, ok, State#state{games = lists:keyreplace(GameId, 2, State#state.games, NewGame)}}
								    end
							end
					end
			end
	end.

%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast({subscribe, {PlayerId, Pid}}, State) ->
	case lists:keyfind(PlayerId, 2, State#state.players) of
		false ->
    		{noreply, State};
		Player ->
		    {noreply, State#state{players = lists:keyreplace(PlayerId, 2, State#state.players, add_subscriber(Pid, Player))}}
	end.

%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
terminate(_Reason, _State) ->
    ok.

%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
insert_player(Id, Name, Players) ->
    [#player{id = Id, name = Name, subscribers = []} | Players].

insert_game(Id, PlayerId, {X, Y}, Games) ->
    [#game{id = Id, player1 = PlayerId, grid = new_grid(PlayerId, {X, Y})} | Games].

new_grid(PlayerId, {1, Y}) ->
	#grid{row1 = new_row(PlayerId, Y), row2 = #row{}, row3 = #row{}};
new_grid(PlayerId, {2, Y}) ->
	#grid{row1 = #row{}, row2 = new_row(PlayerId, Y), row3 = #row{}};
new_grid(PlayerId, {3, Y}) ->
	#grid{row1 = #row{}, row2 = #row{}, row3 = new_row(PlayerId, Y)}.

new_row(PlayerId, 1) ->
	#row{cell1 = PlayerId};
new_row(PlayerId, 2) ->
	#row{cell2 = PlayerId};
new_row(PlayerId, 3) ->
	#row{cell3 = PlayerId}.

move_is_valid(Grid, {1, Y}) ->
	move_is_valid_for_row(Grid#grid.row1, Y);
move_is_valid(Grid, {2, Y}) ->
	move_is_valid_for_row(Grid#grid.row2, Y);
move_is_valid(Grid, {3, Y}) ->
	move_is_valid_for_row(Grid#grid.row3, Y).

move_is_valid_for_row(Row, 1) ->
	Row#row.cell1 == undefined;
move_is_valid_for_row(Row, 2) ->
	Row#row.cell2 == undefined;
move_is_valid_for_row(Row, 3) ->
	Row#row.cell3 == undefined.

join_game(PlayerId, {X, Y}, Game) ->
	Game#game{player2 = PlayerId, grid = update_grid(PlayerId, {X, Y}, Game#game.grid)}.

update_grid(PlayerId, {1, Y}, Grid) ->
	Grid#grid{row1 = update_row(PlayerId, Y, Grid#grid.row1)};
update_grid(PlayerId, {2, Y}, Grid) ->
	Grid#grid{row2 = update_row(PlayerId, Y, Grid#grid.row2)};
update_grid(PlayerId, {3, Y}, Grid) ->
	Grid#grid{row3 = update_row(PlayerId, Y, Grid#grid.row3)}.

update_row(PlayerId, 1, Row) ->
	Row#row{cell1 = PlayerId};
update_row(PlayerId, 2, Row) ->
	Row#row{cell2 = PlayerId};
update_row(PlayerId, 3, Row) ->
	Row#row{cell3 = PlayerId}.

add_subscriber(Pid, Player) ->
	Player#player{subscribers = [Pid | Player#player.subscribers]}.

your_move(Player, Game) ->
    lists:foreach(
		fun(Pid) ->
			% This is bad, raw messaging. Should have a client for this
			% but it would complicate the example
			Pid ! {your_move, {Player, Game}}
		end,
		Player#player.subscribers
	).

