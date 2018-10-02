open Core_kernel
open OUnit

module B = Battleship

let test_place_ship _text_ctx ship position msg =
    let b_option = B.place_ship B.empty_board
                                ship position in
    match b_option with
    | Some b ->
       let l = List.fold_left b
                              ~init:0
                              ~f:(fun a c -> match c with
                                             | _, B.Occupied (_,_) -> a + 1
                                             | _, _ -> a) in
       assert_equal ship.size l ~msg:msg
    | None -> assert_equal false true

let tests =
  "Battleship Tests" >:::
    [
      "empty board" >:: (fun _test_ctxt ->
                         assert_equal ~printer:B.to_string B.empty_board
                                      B.empty_board);

      "place carrier" >:: (fun _test_ctx ->
                           test_place_ship _test_ctx B.carrier (1,'a',B.Down) "Carrier should be 5 squares long");

      "place battleship" >:: (fun _test_ctx ->
                           test_place_ship _test_ctx B.battleship (1,'a',B.Across) "Battleship should be 4 squares long");

      "place submarine" >:: (fun _test_ctx ->
                           test_place_ship _test_ctx B.submarine (1,'a',B.Across) "Submarine should be 3 squares long");

      "place cruiser" >:: (fun _test_ctx ->
                           test_place_ship _test_ctx B.cruiser (1,'a',B.Across) "Cruiser should be 2 squares long");

      "place patrol" >:: (fun _test_ctx ->
                           test_place_ship _test_ctx B.patrol (1,'a',B.Across) "Cruiser should be 1 square long");

      "random board" >:: (fun _test_ctx ->
                          let l = List.fold_left (B.random_board()) ~init:0
                                                 ~f:(fun a c -> match c with
                                                                | _, B.Occupied (_,_) -> a + 1
                                                                | _ -> a) in
                          assert_equal 15 l ~msg:"All ships should be present on a random board" ~printer:string_of_int)
    ]

let _ =
  run_test_tt_main ("all" >::: [tests])
