(* [gamestate] is an abstract type representing the state of an adventure. *)
type building_type = Unbuildable | Dorm | Resource | Connection | Section
type terrain = Water | Forest | Clear | Gorges
type square = {
  btype : building_type;
  level : int;
  xcoord : int;
  ycoord : int;
  maintenance_cost : int;
  terrain : terrain
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


(* [score s] is the player's current score. *)
val score : gamestate -> int

(* [turns s] is the number of turns the player has taken so far. *)
val turns : gamestate -> int

(*getters for all fields*)


val do' : Command.command -> gamestate -> gamestate
