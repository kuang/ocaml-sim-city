type resource =
  | Dining of (int*int) list
  | Lecture of (int*int) list
  | Power of (int*int) list

type terrain = Water | Forest | Clear | Gorges

type disaster = Fire | Blizzard | Prelim

type building_type =
  | Dorm of (int*int) list
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
}

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

let init_state = {
  disaster = None;
  lose = false;
  message = Some ("Welcome to the game!");
  money = 0;
  tuition = 50000;
  happiness = 50;
  time_passed = 0;
  grid = failwith "NO INITIAL GRID"
}
