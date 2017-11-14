(* [gamestate] is an abstract type representing the state of an adventure. *)
type resource = Dining of int*int | Lecture of int*int | Power of int*int
type terrain = Water | Forest | Clear | Gorges
type building_type =
  | Dorm of int*int
  | Resource of resource
  | Road
  | Power (*power lines*)
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
  money : int;
  tuition : int;
  num_turns : int;
  happiness: int;
  grid : square array array
}
(* [init_state j] is the initial state of the game as
 * determined by JSON object [j].
 * requires: [j] represents an error-free adventure file. *)
val init_state : gamestate


val do' : Command.command -> gamestate -> gamestate
