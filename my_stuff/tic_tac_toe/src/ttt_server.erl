-module(ttt_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, player/1, players/0, game/2, games/0, join/3, subscribe/1, move/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(row, {cell1, cell2, cell3}).
-record(grid, {row1, row2, row3}).
-record(players, {player1, player2}).
-record(game, {id, players, grid, state}).
-record(player, {id, name, subscribers}).
-record(state, {players, next_player_id, games, next_game_id}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

player(Name) ->
  gen_server:call(?SERVER, {player, Name}).

players() ->
  gen_server:call(?SERVER, {players}).

game(PlayerId, {X, Y}) when is_integer(X), is_integer(Y), X > 0, X < 4, Y > 0, Y < 4 ->
  gen_server:call(?SERVER, {game, PlayerId, {X, Y}}).

games() ->
  gen_server:call(?SERVER, {games}).

join(PlayerId, GameId, {X, Y}) when is_integer(X), is_integer(Y), X > 0, X < 4, Y > 0, Y < 4 ->
  gen_server:call(?SERVER, {join, PlayerId, GameId, {X, Y}}).

subscribe(PlayerId) ->
  gen_server:cast(?SERVER, {subscribe, {PlayerId, self()}}).

move(PlayerId, GameId, {X, Y}) when is_integer(X), is_integer(Y), X > 0, X < 4, Y > 0, Y < 4 ->
  gen_server:call(?SERVER, {move, PlayerId, GameId, {X, Y}}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{players = [], next_player_id = 0, games = [], next_game_id = 0}}.

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
            Game#game.state /= waiting_for_player2 ->
              {reply, error_game_in_progress, State};
            true ->
              case move_is_valid(Game#game.grid, {X, Y}) of
                false ->
                  {reply, error_invalid_move, State};
                true ->
                  NewGame = join_game(PlayerId, {X, Y}, Game),
                  % notify player1's subscribers that it's their turn
                  your_move(lists:keyfind(Game#game.players#players.player1, 2, State#state.players), NewGame),
                    {reply, ok, State#state{games = lists:keyreplace(GameId, 2, State#state.games, NewGame)}}
              end
          end
      end
  end.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({subscribe, {PlayerId, Pid}}, State) ->
  case lists:keyfind(PlayerId, 2, State#state.players) of
    false ->
      {noreply, State};
    Player ->
      {noreply, State#state{players = lists:keyreplace(PlayerId, 2, State#state.players, add_subscriber(Pid, Player))}}
  end.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

insert_player(Id, Name, Players) ->
  [#player{
    id = Id,
    name = Name,
    subscribers = []
  } | Players].

insert_game(Id, PlayerId, {X, Y}, Games) ->
  [#game{
    id = Id,
    players = #players{player1 = PlayerId},
    grid = new_grid({X, Y}),
    state = waiting_for_player2
  } | Games].

new_grid({1, Y}) ->
  #grid{row1 = new_row(Y), row2 = #row{}, row3 = #row{}};
new_grid({2, Y}) ->
  #grid{row1 = #row{}, row2 = new_row(Y), row3 = #row{}};
new_grid({3, Y}) ->
  #grid{row1 = #row{}, row2 = #row{}, row3 = new_row(Y)}.

new_row(1) ->
  #row{cell1 = 1};
new_row(2) ->
  #row{cell2 = 1};
new_row(3) ->
  #row{cell3 = 1}.

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
  Game#game{
    players = Game#game.players#players{player2 = PlayerId},
    grid = update_grid(2, {X, Y}, Game#game.grid),
    state = waiting_for_move_from_player1
  }.

update_grid(PlayerNumber, {1, Y}, Grid) ->
  Grid#grid{row1 = update_row(PlayerNumber, Y, Grid#grid.row1)};
update_grid(PlayerNumber, {2, Y}, Grid) ->
  Grid#grid{row2 = update_row(PlayerNumber, Y, Grid#grid.row2)};
update_grid(PlayerNumber, {3, Y}, Grid) ->
  Grid#grid{row3 = update_row(PlayerNumber, Y, Grid#grid.row3)}.

update_row(PlayerNumber, 1, Row) ->
  Row#row{cell1 = PlayerNumber};
update_row(PlayerNumber, 2, Row) ->
  Row#row{cell2 = PlayerNumber};
update_row(PlayerNumber, 3, Row) ->
  Row#row{cell3 = PlayerNumber}.

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
