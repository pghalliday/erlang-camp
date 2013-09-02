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
-record(game, {id, players, grid, next_player, state, winner, move_count}).
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
  get_player(PlayerId, State, fun(_Player) ->
    Id = State#state.next_game_id,
    {reply, Id, State#state{games = insert_game(Id, PlayerId, {X, Y}, State#state.games), next_game_id = Id + 1}}
  end);
handle_call({games}, _From, State) ->
  {reply, State#state.games, State};
handle_call({join, PlayerId, GameId, Move}, _From, State) ->
  get_player(PlayerId, State, fun(_Player) ->
    get_game(GameId, State, fun(Game) ->
      is_correct_state(Game, waiting_for_player2, error_not_waiting_for_player2, State, fun() ->
        is_move_valid(Game, Move, State, fun() ->
          NewGame = join_game(PlayerId, Move, Game),
          % notify player1's subscribers that it's their turn
          get_player(Game#game.players#players.player1, State, fun(Player) ->
            your_move(Player, NewGame),
            {reply, ok, State#state{games = lists:keyreplace(GameId, 2, State#state.games, NewGame)}}
          end)
        end)
      end) 
    end)
  end);
handle_call({move, PlayerId, GameId, Move}, _From, State) ->
  get_player(PlayerId, State, fun(_Player) ->
    get_game(GameId, State, fun(Game) ->
      is_correct_state(Game, in_progress, error_game_is_not_in_progress, State, fun() ->
        is_correct_player(Game, PlayerId, State, fun(ThisPlayerNumber, OtherPlayerNumber) ->
          is_move_valid(Game, Move, State, fun() ->
            NewGame = play_move(ThisPlayerNumber, OtherPlayerNumber, Move, Game),
            NewState = State#state{games = lists:keyreplace(GameId, 2, State#state.games, NewGame)},
            get_player_id(Game#game.players, OtherPlayerNumber, fun(OtherPlayerId) ->
              get_player(OtherPlayerId, NewState, fun(Player) ->
                is_game_still_in_progress(ThisPlayerNumber, Player, GameId, NewGame, NewState, fun() ->
                  % notify other players subscribers that it's their turn
                  notify(Player, your_move, NewGame),
                  {reply, ok, NewState}
                end)
              end)
            end)
          end)
        end)
      end)
    end)
  end).

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
    next_player = player2,
    state = waiting_for_player2,
    move_count = 1
  } | Games].

new_grid({1, Y}) ->
  #grid{row1 = new_row(Y), row2 = #row{}, row3 = #row{}};
new_grid({2, Y}) ->
  #grid{row1 = #row{}, row2 = new_row(Y), row3 = #row{}};
new_grid({3, Y}) ->
  #grid{row1 = #row{}, row2 = #row{}, row3 = new_row(Y)}.

new_row(1) ->
  #row{cell1 = player1};
new_row(2) ->
  #row{cell2 = player1};
new_row(3) ->
  #row{cell3 = player1}.

is_move_valid(Game, Move, State, Callback) ->
  case is_move_valid(Game#game.grid, Move) of
    false ->
      {reply, error_invalid_move, State};
    true ->
      Callback()
  end.

is_move_valid(Grid, {1, Y}) ->
  is_move_valid_for_row(Grid#grid.row1, Y);
is_move_valid(Grid, {2, Y}) ->
  is_move_valid_for_row(Grid#grid.row2, Y);
is_move_valid(Grid, {3, Y}) ->
  is_move_valid_for_row(Grid#grid.row3, Y).

is_move_valid_for_row(Row, 1) ->
  Row#row.cell1 == undefined;
is_move_valid_for_row(Row, 2) ->
  Row#row.cell2 == undefined;
is_move_valid_for_row(Row, 3) ->
  Row#row.cell3 == undefined.

join_game(PlayerId, {X, Y}, Game) ->
  Game#game{
    players = Game#game.players#players{player2 = PlayerId},
    grid = update_grid(player2, {X, Y}, Game#game.grid),
    next_player = player1,
    state = in_progress,
    move_count = 2
  }.

play_move(ThisPlayerNumber, OtherPlayerNumber, Move, Game) ->
  Game#game{
    grid = update_grid(ThisPlayerNumber, Move, Game#game.grid),
    next_player = OtherPlayerNumber,
    move_count = Game#game.move_count + 1
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

get_player(PlayerId, State, Callback) ->
  case lists:keyfind(PlayerId, 2, State#state.players) of
    false ->
      {reply, error_no_player, State};
    Player ->
      Callback(Player)
  end.

get_game(GameId, State, Callback) ->
  case lists:keyfind(GameId, 2, State#state.games) of
    false ->
      {reply, error_no_game, State};
    Game ->
      Callback(Game)
  end.

is_correct_state(Game, RequiredState, Error, State, Callback) ->
  if
    Game#game.state /= RequiredState ->
      {reply, Error, State};
    true ->
      Callback()
  end.

is_correct_player(Game, PlayerId, State, Callback) ->
  get_next_player_id(Game, fun(NextPlayerId, ThisPlayerNumber, OtherPlayerNumber) ->
    case NextPlayerId of
      PlayerId ->
        Callback(ThisPlayerNumber, OtherPlayerNumber);
      _WRONG_PLAYER ->
        {reply, error_not_your_move, State}
    end
  end).

get_player_id(Players, PlayerNumber, Callback) ->
  case PlayerNumber of
    player1 ->
      Callback(Players#players.player1);
    player2 ->
      Callback(Players#players.player2)
  end.

get_next_player_id(Game, Callback) ->
  case Game#game.next_player of
    player1 ->
      Callback(Game#game.players#players.player1, player1, player2);
    player2 ->
      Callback(Game#game.players#players.player2, player2, player1)
  end.

is_game_still_in_progress(PlayerNumber, OtherPlayer, GameId, Game, State, Callback) ->
  is_winning_line(PlayerNumber, OtherPlayer, {Game#game.grid#grid.row1#row.cell1, Game#game.grid#grid.row1#row.cell2, Game#game.grid#grid.row1#row.cell3}, GameId, Game, State, fun() ->
    is_winning_line(PlayerNumber, OtherPlayer, {Game#game.grid#grid.row2#row.cell1, Game#game.grid#grid.row2#row.cell2, Game#game.grid#grid.row2#row.cell3}, GameId, Game, State, fun() ->
      is_winning_line(PlayerNumber, OtherPlayer, {Game#game.grid#grid.row3#row.cell1, Game#game.grid#grid.row3#row.cell2, Game#game.grid#grid.row3#row.cell3}, GameId, Game, State, fun() ->
        is_winning_line(PlayerNumber, OtherPlayer, {Game#game.grid#grid.row1#row.cell1, Game#game.grid#grid.row2#row.cell1, Game#game.grid#grid.row3#row.cell1}, GameId, Game, State, fun() ->
          is_winning_line(PlayerNumber, OtherPlayer, {Game#game.grid#grid.row1#row.cell2, Game#game.grid#grid.row2#row.cell2, Game#game.grid#grid.row3#row.cell2}, GameId, Game, State, fun() ->
            is_winning_line(PlayerNumber, OtherPlayer, {Game#game.grid#grid.row1#row.cell3, Game#game.grid#grid.row2#row.cell3, Game#game.grid#grid.row3#row.cell3}, GameId, Game, State, fun() ->
              is_winning_line(PlayerNumber, OtherPlayer, {Game#game.grid#grid.row1#row.cell1, Game#game.grid#grid.row2#row.cell2, Game#game.grid#grid.row3#row.cell3}, GameId, Game, State, fun() ->
                is_winning_line(PlayerNumber, OtherPlayer, {Game#game.grid#grid.row1#row.cell3, Game#game.grid#grid.row2#row.cell2, Game#game.grid#grid.row3#row.cell1}, GameId, Game, State, fun() ->
                  case Game#game.move_count of
                    9 ->
                      % notify other players subscribers of stalemate
                      NewGame = Game#game{state = game_over},
                      NewState = State#state{games = lists:keyreplace(GameId, 2, State#state.games, NewGame)},
                      notify(OtherPlayer, stalemate, NewGame),
                      {reply, stalemate, NewState};
                    _NOT_A_STALEMATE ->
                      Callback()
                  end
                end)
              end)
            end)
          end)
        end)
      end)
    end)
  end).

is_winning_line(PlayerNumber, OtherPlayer, Cells, GameId, Game, State, Callback) ->
  case Cells of
    {PlayerNumber, PlayerNumber, PlayerNumber} ->
      % notify other players subscribers that they lost
      NewGame = Game#game{state = game_over, winner = PlayerNumber},
      NewState = State#state{games = lists:keyreplace(GameId, 2, State#state.games, NewGame)},
      notify(OtherPlayer, you_lost, NewGame),
      {reply, you_win, NewState};
    _NOT_A_WINNING_LINE ->
      Callback()
  end.

notify(Player, Message, Game) ->
  lists:foreach(
    fun(Pid) ->
      % This is bad, raw messaging. Should have a client for this
      % but it would complicate the example
      Pid ! {Message, {Player, Game}}
    end,
    Player#player.subscribers
  ).



