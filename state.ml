type building_type = Unbuildable | Dorm | Resource | Connection | Section

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
