(* [resource] is a type encompassing the different resources: Dining, Lecture,
 * and Power. It contains information about the squares that the resource
 * uses.*)
type resource =
  | Dining of (int*int) list
  | Lecture of (int*int) list
  | Power of (int*int) list

(* [terrain] represents the different features of the natural landscape of the
 * game map. *)
type terrain = Water | Forest | Clear | Gorges

(* [disaster] represents the different types of natural disaster events. *)
type disaster = Fire | Blizzard | Prelim

(* [building_type] is a type encompassing the different representations of a
 * "building". Dorm and Section contain information about the squares they
 * use, and represent dorm and ???. Resource represents resource buildings;
 * respectively, Road and Pline represent roads and power line connecitons.
 * When there is nothing built on a square, the building is Empty. *)
type building_type =
  | Dorm of (int*int) list
  | Resource of resource
  | Road
  | Pline (*power lines*)
  | Section of int*int
  | Empty

(* [square] is a type representing the current state of an individual game
   square. It contains information pertaining to what is currently on the
   square (represented by field btype), and also attributes that are dependent
   on btype (ex. maintenance_cost, level). Finally, this type also keeps track
   of "natural" and "static" information about the square itself, such as its x
   and y coordinates and terrain features. *)
type square = {
  btype : building_type;
  level : int;
  xcoord : int;
  ycoord : int;
  maintenance_cost : int;
  terrain : terrain;

}

(* [gamestate] is a type representing the state of an adventure. It contains
   all the information necessary to recreate the current state of the game,
   including overall information (current money, happiness etc) and individual
   square information (what is current built on each square, resource
   connections, etc). The overall game information is stored as record fields,
   while the square information is stored as a 2D array of type square
   elements. *)
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

(* [init_state j] returns the initial state of the game. *)
val init_state : gamestate

(* [do' command gamestate] applies [command] to [gamestate] and returns a
   gamestate object representing the new state of game. *)
val do' : Command.command -> gamestate -> gamestate
