open Command

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

let do_build x y b =
  failwith "Unimplemented"

let do_delete x y =
  failwith "Unimplemented"

let do_tuition n =
  failwith "Unimplemented"

let do_time =
  failwith "Unimplemented"

let do' (c:command) =
  match c with
  | Build (x,y,b) -> do_build x y b
  | Delete (x,y) -> do_delete x y
  | SetTuition n -> do_tuition n
  | TimeStep -> do_time
