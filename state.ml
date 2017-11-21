open Command

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
  maintenance_cost : int;
  population: int;
  (* xcoord : int; *)
  (* ycoord : int; *)
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

let init_square = {
  btype = Empty;
  level = 0;
  maintenance_cost = 0;
  population= 0;
  (* xcoord : int; *)
  (* ycoord : int; *)
}
let init_state (grid_size:int)= {
  disaster = None;
  lose = false;
  message = Some ("Welcome to the game!");
  money = 0;
  tuition = 50000;
  happiness = 50;
  time_passed = 0;
  grid = Array.make grid_size (Array.make grid_size init_square);
}

let do_build x y b st =
  failwith "Unimplemented"

let do_delete x y st =
  failwith "Unimplemented"

let do_tuition n st =
  {
    st with tuition = n;
            happiness = let updated_happiness =
                          st.happiness - ((n - st.tuition)/100) in
              if updated_happiness < (-100) then -100
              else if updated_happiness > 100 then 100
              else updated_happiness
  }

let do_time st =
  failwith "Unimplemented"

let do' (c:Command.command) st =
  match c with
  | Build (x,y,b) -> do_build x y b st
  | Delete (x,y) -> do_delete x y st
  | SetTuition n -> do_tuition n st
  | TimeStep -> do_time st
