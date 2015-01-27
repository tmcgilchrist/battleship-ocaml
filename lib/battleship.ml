open Core.Std

type ship_type = Carrier
               | Battleship
               | Submarine
               | Cruiser
               | Patrol

type ship = { ship_type : ship_type; size : int; }
type position = Occupied of ship_type * bool
              | Unoccupied

type direction = Across | Down

type board = (int * char, position) Core.Std.List.Assoc.t

let carrier    = {ship_type = Carrier;    size = 5}
let battleship = {ship_type = Battleship; size = 4}
let submarine  = {ship_type = Submarine;  size = 3}
let cruiser    = {ship_type = Cruiser;    size = 2}
let patrol     = {ship_type = Patrol;     size = 1}

let all_ships = [carrier; battleship; submarine; cruiser; patrol]

let columns = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j']

let random_direction =
  match Random.bool() with
  | true -> Down
  | false -> Across

let int_in_range i size =
  ((Int.between i 0 9) &&
     (Int.between (i + size) 1 9))

let char_in_range c size =
  (Char.between c 'a' 'j') &&
    (Char.between(Char.of_int_exn(Char.to_int(c) + size)) 'a' 'j')

let positions_unoccupied board p =
  List.fold_left p ~init:false
                 ~f:(fun a c -> match (List.Assoc.find board c) with
                                | Some (Unoccupied)   -> a
                                | Some (Occupied (_)) -> a || true
                                | None                -> a)

let place_ship board ship p =
  let place_ship_at board ps ship =
    List.fold_left ps ~init:board
                   ~f:(fun b a ->
                       let minus = List.Assoc.remove b a in
                       List.Assoc.add minus a (Occupied(ship.ship_type, false))) in
  let gen_positions = function
    | (x, y, Down) ->
       List.map (List.range x (x+ship.size)) ~f: (fun a -> (a, y))
    | (x, y, Across) ->
       let yi = (Char.to_int y) in
       List.map (List.range yi (yi+ship.size))
                ~f: (fun a -> (x, Char.of_int_exn a)) in

  let valid_position (x, y, direction) size =
    ((int_in_range x size) && (char_in_range y size)) in

  match (valid_position p ship.size) with
  | true ->
     let ps = gen_positions p in
     (match (positions_unoccupied board ps) with
     | false ->
        Some (place_ship_at board ps ship)
     | true ->
        None)
  | false -> None

let empty_board =
  let row x =
    List.map columns ~f:(fun y -> ((x, y), Unoccupied)) in
  List.join(List.map (List.range 0 10) ~f:row);;

let rec random_ship board ship =
  let x = Random.int(10) in
  let y = List.nth_exn columns (Random.int (10 - ship.size)) in
  match (place_ship board ship (x,y,random_direction)) with
  | Some b -> b
  | None -> random_ship board ship

let random_board() =
  List.fold_left all_ships ~init: empty_board ~f:random_ship

let attack board a =
  match List.Assoc.find board a with
  | Some (Occupied (t, _)) ->
     let minus = List.Assoc.remove board a in
     let new_board = List.Assoc.add minus a (Occupied(t, true)) in
     Some new_board
  | Some (Unoccupied) -> None
  | None -> None

let finished board =
  let hit h p = match p with
    | (x,y), Occupied (t, true) -> h+1
    | (x,y), Occupied (t, false) -> h
    | (x,y), Unoccupied -> h in
  let hits = List.fold_left board ~init:0 ~f:hit in
  hits = List.fold_left all_ships ~init:0 ~f:(fun a s -> a + s.size)

(********************** Scratch / Util **********************)

let print_cell str cell =
  let new_line col row = match col with
    | 'j' -> "\n"
    | _ -> "" in
  match cell with
  | (x,y), Occupied (ship_type, hit) ->
     sprintf "%s S(%i,%c) %s" str x y (new_line y x)
  | (x,y), Unoccupied                ->
     sprintf "%s U(%i,%c) %s" str x y (new_line y x)

(** Sort the board pairwise *)
let sort_board board =
  let lex_cmp (x,y) (x',y') =
    let compare_fst = compare x x' in
    if compare_fst <> 0 then compare_fst
    else compare y y' in
  List.sort ~cmp:lex_cmp board

let to_string board =
  let sorted_board = sort_board board in
  List.fold_left sorted_board ~init: "" ~f:print_cell

let print_board board =
  let heading = List.fold_left columns ~init:""
                               ~f:(fun a c -> sprintf"%s%c       " a c) in
  Printf.printf "Current board \n %s\n\n%s\n" heading (to_string board)
