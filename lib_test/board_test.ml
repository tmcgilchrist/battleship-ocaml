open Core.Std
open OUnit2
module B = Battleship

let test_place_ship text_ctx ship position msg =
    let b_option = B.place_ship B.empty_board
                                ship position in
    match b_option with
    | Some b ->
       let l = List.fold_left b
                              ~init:0
                              ~f:(fun a c -> match c with
                                             | p, B.Occupied (_,_) -> a + 1
                                             | _, _ -> a) in
       assert_equal ship.size l ~msg:msg
    | None -> assert_true false

let tests =
  "Battleship Tests" >:::
    [
      "empty board" >:: (fun test_ctxt ->
                         assert_equal ~printer:B.to_string B.empty_board
                                      B.empty_board);

      "place carrier" >:: (fun test_ctx ->
                           test_place_ship test_ctx B.carrier (1,'a',B.Down) "Carrier should be 5 squares long");

      "place battleship" >:: (fun test_ctx ->
                           test_place_ship test_ctx B.battleship (1,'a',B.Across) "Battleship should be 4 squares long");

      "place submarine" >:: (fun test_ctx ->
                           test_place_ship test_ctx B.submarine (1,'a',B.Across) "Submarine should be 3 squares long");

      "place cruiser" >:: (fun test_ctx ->
                           test_place_ship test_ctx B.cruiser (1,'a',B.Across) "Cruiser should be 2 squares long");

      "place patrol" >:: (fun test_ctx ->
                           test_place_ship test_ctx B.patrol (1,'a',B.Across) "Cruiser should be 1 square long");

      "random board" >:: (fun test_ctx ->
                          let l = List.fold_left (B.random_board()) ~init:0
                                                 ~f:(fun a c -> match c with
                                                                | (x,y), B.Occupied (_,_) -> a + 1
                                                                | _ -> a) in
                          assert_equal 15 l ~msg:"All ships should be present on a random board" ~printer:string_of_int)
    ]

let () =
  run_test_tt_main ("all" >::: [tests])
