(* open Command *)

(* [terrain] represents the different features of the natural landscape of the
 * game map. *)
type terrain = Water | Forest | Clear

(* [disaster] represents the different types of natural disaster events. *)
type disaster = Fire | Blizzard | Prelim

(* [building_type] is a type encompassing the different options for what can be
 * "built" on a square. Dorm represents dorm buildings, Resource represents
 * resource buildings, Road and Pline represent roads and power line
 * connections. When there is nothing built on a square, the building is Empty.
 * Dorm and Resource buildings are multiple squares in size, so the Section
 * variant exists to represent squares that are part of a larger building. *)
 type building_type =
   | Dorm
   | Dining
   | Lecture
   | Power
   | Park
   | Road
   | Pline (*power lines*)
   | Section of int*int
   | Empty

(* Represents commands to be executed on the state. Build, Delete, and
 * SetTuition are commands issued by the user to build or destroy a building at
 * specific coordinates, or to change the university's tuition rate. TimeStep
 * is a command issued automatically at regular time intervals that allows time
 * to pass in     the game state. *)
type command =
  | Build of int*int*building_type
  | Delete of int*int
  | SetTuition of int
  | TimeStep

(* [square] is a type representing the current state of an individual game
 * square. It contains information pertaining to what is currently on the
 * square (represented by field btype), and also attributes that are dependent
 * on btype (ex. maintenance_cost, level). Finally, this type also keeps track
 * of "natural" and "static" information about the square itself, such as its x
 * and y coordinates and terrain features. *)
type square = {
  btype : building_type;
  level : int;
  maintenance_cost : int;
  population: int;
  terrain : terrain;
  dining_access: bool;
  lec_access: bool;
  power_access: bool;
}

(* [gamestate] is a type representing the state of an adventure. It contains
 * all the information necessary to recreate the current state of the game,
 * including overall information (current money, happiness etc) and individual
 * square information (what is current built on each square, resource
 * connections, etc). The overall game information is stored as record fields,
 * while the square information is stored as a 2D array of type square
 * elements. *)
type gamestate = {
  disaster : disaster option;
  lose : bool;
  message : string option; (*possible prompt for the user*)
  money : int;
  tuition : int;
  happiness: int;
  time_passed : int;
  grid : square array array
}

(* [init_state] returns the initial state of the game. *)
val init_state : int -> gamestate

(* [init_from_file] returns a state parsed from the file named filename. *)
val init_from_file : string -> gamestate option

(*exposed getter for happiness field.*)
val get_happiness : gamestate -> int

(*exposed getter for money field.*)
val get_money : gamestate -> int

(*exposed getter for tuition field.*)
val get_tuition : gamestate -> int

(*exposed getter for time_passed field.*)
val get_time_passed : gamestate -> int

(*exposed getter for disaster field.*)
val get_disaster : gamestate -> disaster option

(*exposed getter for message field.*)
val get_message : gamestate -> string option

(*exposed getter for lose field.*)
val get_lose : gamestate -> bool

(* [get_rpop row] returns the total population of all squares in [row]. *)
val get_rpop : square array -> int

(* [get_num st f] is the sum of [f x] over all squares [x] in [st.grid] *)
val get_num : square array array -> (square array -> int) -> int

(* [do' command gamestate] applies [command] to [gamestate] and returns a
 * gamestate object representing the new state of game. *)
val do' : command -> gamestate -> gamestate
