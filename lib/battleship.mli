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

type board = ((int * char) * position) list

type direction = Across | Down
type point = int * char * direction

val carrier :  ship
val battleship : ship
val submarine : ship
val cruiser : ship
val patrol : ship

val empty_board : board
val random_board : board
val place_ship : board -> ship -> (int * char * direction) -> board option
val attack : board -> (int * char) -> board
val to_string: board -> string
