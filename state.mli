(* [gamestate] is an abstract type representing the state of an adventure. *)
type resource =
  | Dining of int*int list
  | Lecture of int*int list
  | Power of int*int list
type terrain = Water | Forest | Clear | Gorges
type disaster = Fire | Blizzard | Prelim
type building_type =
  | Dorm of int*int list
  | Resource of resource
  | Road
  | Pline (*power lines*)
  | Section of int*int
  | Empty

type square = {
  btype : building_type;
  level : int;
  xcoord : int;
  ycoord : int;
  maintenance_cost : int;
  terrain : terrain;
}

type gamestate = {
  disaster : disaster option;
  money : int;
  tuition : int;
  happiness: int;
  time_passed : int;
  grid : square array array
}

(* [init_state j] returns the initial state of the game. *)
val init_state : gamestate


val do' : Command.command -> gamestate -> gamestate
