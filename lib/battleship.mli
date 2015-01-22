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

type board = (int * char * position) list

val carrier :  ship
val battleship : ship
val submarine : ship
val cruiser : ship
val patrol : ship

val place_ship : (int * char, position) Core.Std.List.Assoc.t ->
                 ship ->
                 int * char * direction ->
                 (int * char, position) Core.Std.List.Assoc.t option
val empty_board : ((int * char) * position) list
val random_ship : (int * char, position) Core.Std.List.Assoc.t ->
                  ship -> (int * char, position) Core.Std.List.Assoc.t
val random_board : unit -> ((int * char) * position) list
val attack : ('a, position) Core.Std.List.Assoc.t ->
             'a -> ('a, position) Core.Std.List.Assoc.t
val finished : position list -> bool
val print_cell : bytes -> (int * char) * position -> bytes
val to_string : ((int * char) * position) list -> bytes
val print_board : ((int * char) * position) list -> unit
