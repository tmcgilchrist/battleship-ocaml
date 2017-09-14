open Core_kernel

type ship_type = Carrier
               | Battleship
               | Submarine
               | Cruiser
               | Patrol
               [@@deriving compare]

type ship = { ship_type : ship_type; size : int; }
type position = Occupied of ship_type * bool
              | Unoccupied
              [@@deriving compare]

type direction = Across | Down

type board = (int * char, position) List.Assoc.t

val carrier :  ship
val battleship : ship
val submarine : ship
val cruiser : ship
val patrol : ship

(** Places a ship at position x,y, Down|Across returns Some(board) on success *)
(** otherwise None if the position is invalid or already occupied *)
val place_ship : board ->
                 ship ->
                 int * char * direction ->
                 board option

(** Generate an empty 10*10 board *)
val empty_board : board

(** Generate a board with ships placed randomly *)
val random_board : unit -> board

(** attacks a position, retuning Some(board) if successful otherwise None *)
val attack : board ->
             int * char ->
             board option
(** Given a players attacking board returns bool whether all ships have been *)
(** hit*)
val finished : board -> bool

(** Prints a board as a string for testing *)
val to_string : board -> string

(** Prints a formatted board to stdout *)
val print_board : board -> unit
