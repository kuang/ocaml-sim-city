type building_type = Unbuildable | Dorm | Resource | Connection | Section

type square = {
  btype : building_type;
  level : int;
  xcoord : int;
  ycoord : int;
  maintenance_cost : int;
}

type gamestate = {
  money : int;
  tution : int;
  num_turns : int;
  happiness: int;
  grid : square array array
}
