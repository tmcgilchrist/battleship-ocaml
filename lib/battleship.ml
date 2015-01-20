open Core.Std

type ship_type = Carrier
               | Battleship
               | Submarine
               | Cruiser
               | Patrol

type ship = { ship_type : ship_type; size : int; }
type position = Occupied of ship_type * bool
              | Unoccupied
              | Hit
              | Miss

type direction = Across | Down

type point = int * char * direction

type board = ((int * char) * position) list

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

let place_ship board ship p =
  let char_in_range c =
    (Char.between c 'a' 'j') &&
       (Char.between(Char.of_int_exn(Char.to_int(c) + ship.size)) 'a' 'j') in
  let int_in_range i r =
    ((Int.between i 0 9) && (Int.between (i + ship.size) 1 9)) in
  match p with
  | (x, y, Down) ->
     (match (int_in_range x 9) with
     | true ->
        (* position from x,y going down (increasing x) *)
        let ps = List.map (List.range x (x+ship.size)) ~f: (fun a -> (a, y)) in

        (* lookup each of those positions and place the ship there *)
        Some (List.fold_left ps ~init:board ~f:(fun b a ->
                                                let minus = List.Assoc.remove b a in
                                                List.Assoc.add minus a (Occupied(ship.ship_type, false))))
     | false -> None)
  | (x, y, Across) ->
     (match (char_in_range y) with
     | true ->
        (* position from x,y going down (increasing x) *)
        let ps = List.map (List.range x (x+ship.size)) ~f: (fun a -> (a, y)) in

        (* lookup each of those positions and place the ship there *)
        Some (List.fold_left ps
                             ~init:board
                             ~f:(fun b a -> let minus = List.Assoc.remove b a in
                                            List.Assoc.add minus a (Occupied(ship.ship_type, false))))
     | false -> None)


let empty_board =
  let row x =
    List.map columns ~f:(fun y -> ((x, y), Unoccupied)) in
  let rows = List.map (List.range 0 10) ~f:row in
  List.join(rows);;

let rec random_ship board ship =
  let x = Random.int(10) in
  let y = List.nth_exn columns (Random.int (10 - ship.size)) in
  match (place_ship board ship (x,y,random_direction)) with
  | Some b -> b
  | None -> random_ship board ship

let random_board =
  List.fold_left all_ships ~init: empty_board ~f:random_ship

let attack board a =
  match List.Assoc.find_exn board a with
  | Occupied (t, _) ->
     let minus = List.Assoc.remove board a in
     List.Assoc.add minus a (Occupied(t, true))
  | Unoccupied -> board
  | Hit -> board (* Should disallow these last 2 states *)
  | Miss -> board



let finished board =
  false

(********************** Scratch / Util **********************)



let print_cell str cell =
  let new_line col = match col with
    | 'j' -> "\n"
    | _ -> "" in
  match cell with
  | (x,y), Occupied (ship_type, hit) ->
     sprintf "%s S(%i,%c) %s" str x y (new_line y)
  | (x,y), Unoccupied                -> str ^ " U    " ^ new_line(y)
  | (x,y), Hit                       -> str ^ " H    " ^ new_line(y)
  | (x, y), Miss                     -> str ^ " M    " ^ new_line(y)

let to_string board =
  List.fold_left board ~init:"" ~f:(fun acc e -> print_cell acc  e)

let print_board board =
  let formated_board = List.fold_left board ~init: "" ~f:print_cell in
  Printf.printf "Current board \n ";
  List.iter columns (printf "%c") ;
  Printf.printf "\n";
  Printf.printf"%s \n" formated_board

let sort_pairs c1 c2 =
  let (x,y), _ = c1 in
  let (a,b), _ = c2 in
  x < a || (x = a && y < b)



let findi x l =
  let rec loop i n l =
    match l with
    | y::tl -> loop (i+1) (if y = x then i else n) tl
    | [] -> n
  in
  loop 0 (-1) l

let is_battleship b =
  List.fold_left b ~init:0 ~f:(fun acc e -> match e with
                                             |(_, Occupied(Battleship, _)) -> acc + 1
                                             |_ -> acc)
