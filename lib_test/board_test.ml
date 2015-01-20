open Core.Std
open OUnit2
module B = Battleship

let aeb exp got _test_ctxt = assert_equal ~printer:B.to_string exp got

let tests =
  [
    "empty_board">::
      aeb B.empty_board B.empty_board
  ]

let () =
  run_test_tt_main ("board-ops tests" >::: tests)



(* (\* *)
(*  Test board related functions like creating board, placing ships, etc *)
(*  *\) *)
(* type player = { *)
(*     attack_b: board; *)
(*     defense_b: board; *)
(*   } *)

(* (\* *)

(*  *\) *)

(* let player_1 = *)
(*   let board_1 = random_board() in *)
(*   let board_2 = empty_board() in *)
(*   { attack_b = board_2; *)
(*     defense_b = board_1;} *)

(* let player_2 = *)
(*   let board_1 = random_board() in *)
(*   let board_2 = empty_board() in *)
(*   { attack_b = board_2; *)
(*     defense_b = board_1;} *)

(* let _ = *)
