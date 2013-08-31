-module(editor).
-export([edit/2]).

edit(Source, Commands) ->
  apply_command(Source, Commands, []).

apply_command(Source, [], Output) ->
  lists:append(lists:reverse(Output), Source);
apply_command([_Character | Remainder], [delete | Commands], Output) ->
  apply_command(Remainder, Commands, Output);
apply_command([Character | Remainder], [skip | Commands], Output) ->
  apply_command(Remainder, Commands, [Character | Output]);
apply_command([_Character | Remainder], [{replace, ReplaceCharacter} | Commands], Output) ->
  apply_command(Remainder, Commands, [ReplaceCharacter | Output]);
apply_command(Source, [{insert, InsertCharacter} | Commands], Output) ->
  apply_command([InsertCharacter | Source], Commands, Output);
apply_command(Source, [{add, AddCharacter} | Commands], Output) ->
  apply_command(Source, Commands, [AddCharacter | Output]);
apply_command([], [_Command | Commands], Output) ->
  apply_command([], Commands, Output).
