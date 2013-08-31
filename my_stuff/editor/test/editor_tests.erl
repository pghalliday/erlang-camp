-module(editor_tests).
-include_lib("eunit/include/eunit.hrl").

edit_delete_test() ->
  ?assertEqual("est", editor:edit("test", [delete])).

edit_delete_empty_test() ->
  ?assertEqual("", editor:edit("", [delete])).

edit_delete_twice_test() ->
  ?assertEqual("st", editor:edit("test", [delete, delete])).

edit_skip_test() ->
  ?assertEqual("tst", editor:edit("test", [skip, delete])).

edit_skip_empty_test() ->
  ?assertEqual("", editor:edit("", [skip, delete])).

edit_skip_twice_test() ->
  ?assertEqual("tet", editor:edit("test", [skip, skip, delete])).

edit_replace_test() ->
  ?assertEqual("best", editor:edit("test", [{replace, $b}])).

edit_replace_empty_test() ->
  ?assertEqual("", editor:edit("", [{replace, $b}])).

edit_replace_twice_test() ->
  ?assertEqual("bust", editor:edit("test", [{replace, $b}, {replace, $u}])).

edit_insert_test() ->
  ?assertEqual("itest", editor:edit("test", [{insert, $i}])).

edit_insert_empty_test() ->
  ?assertEqual("i", editor:edit("", [{insert, $i}])).

edit_insert_twice_test() ->
  ?assertEqual("retest", editor:edit("test", [{insert, $e}, {insert, $r}])).

edit_add_test() ->
  ?assertEqual("itest", editor:edit("test", [{add, $i}])).

edit_add_empty_test() ->
  ?assertEqual("i", editor:edit("", [{add, $i}])).

edit_add_twice_test() ->
  ?assertEqual("retest", editor:edit("test", [{add, $r}, {add, $e}])).
