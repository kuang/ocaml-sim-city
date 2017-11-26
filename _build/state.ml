open Command

type terrain = Water | Forest | Clear | Gorges

type disaster = Fire | Blizzard | Prelim

(*global maintenance costs*)
let road_mcost = 10
let pline_mcost = 10
let dorm_mcost = 300
let resource_mcost = 300
let section_mcost = 0
let empty_mcost = 0

(*global build costs*)
let road_bcost = 50
let pline_bcost = 50
let dorm_bcost = 7000
let resource_bcost = 7000
let section_bcost = 0
let empty_bcost = 0


type building_type =
  | Dorm of (int*int) list
  | Dining of (int*int) list
  | Lecture of (int*int) list
  | Power of (int*int) list
  | Road
  | Pline (*power lines*)
  | Section of int*int
  | Empty

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
  terrain = Clear;
  dining_access = false;
  lec_access = false;
  power_access = false;
}

let init_state (grid_size:int)= {
  disaster = None;
  lose = false;
  message = Some ("Welcome to the game!");
  money = 20;
  tuition = 5;
  happiness = 50;
  time_passed = 0;
  grid = Array.make grid_size (Array.make grid_size init_square);
}

(* [gen_disaster] has a small pseudo-random chance of returning [Some disaster],
 * else returns [None] *)
let gen_disaster num =
  Random.init num;
  let fire = Random.int 50 = 1 in
  let bliz = Random.int 70 = 1 in
  let prelim = Random.int 40 = 1 in
  if fire then Some Fire else if bliz then Some Blizzard
  else if prelim then Some Prelim else None

(* [get_rpop row] is the total population of all squares in [row] *)
let get_rpop row =
  Array.fold_left (fun (acc:int) b  -> b.population + acc) 0 row

let get_rmain row =
  Array.fold_left (fun (acc:int) b -> b.maintenance_cost + acc) 0 row

(* [get_num st] is the total population of all squares in [grid] *)
let get_num grid f : int =
  Array.fold_left (fun (acc:int) r -> f r + acc) 0 grid

(* [check_resources st x y] is [true] if this square is connected to
 * all resources, [false] otherwise *)
let check_resources st x y =
  failwith "Unimplemented"

(* [update_build happ b] is [b'], where [b'] is [b] after a month with
 * happiness level [happ]. *)
let update_build happ (b : square) =
  match b.btype with
  | Dorm _ -> begin
      let newpop = b.population + (b.level+1)*happ (* MADE UP NUMBERS*) in {
    b with btype = b.btype;
    level = newpop / 500; (* MADE UP NUMBERS*)
    maintenance_cost = newpop*40;  (* MADE UP NUMBERS*)
    population = newpop;
    }
    end
  | _ -> b

(* [update_row happ r] is [r'] containing squares which have stepped one
 * month under happiness level [happ]. *)
let update_row happ r =
  Array.map (update_build happ) r

(* [update_grid happ grid] is [grid'] containing squares which have stepped one
 * month under happiness level [happ]. *)
let update_grid happ (grid:square array array) =
  Array.map (update_row happ) grid

(*TODO: will call place_building x y b st and then check resources and stuff*)
let do_build x y b st : gamestate = st

(*This following code literally only places a building on the specific
  square, doesn't implement any resource connection stuff*)
let place_building (x:int) (y:int) (b:building_type) st : gamestate =
  let curr_square = st.grid.(x).(y) in
  match b with
  | Dorm lst -> st
  | Lecture lst -> st
  | Power lst -> st
  | Dining lst -> st
  | Road -> (
      let new_square = {
        curr_square with
        btype = Road;
        level = 1;
        maintenance_cost = road_mcost;
        population = 0;
      } in
      let _ =  st.grid.(x).(y) <- new_square in
      st
    )
  | Pline -> st
  | Section (x,y) -> st
  | Empty -> st


let do_delete x y st =
  failwith "Unimplemented"

(* returns: [do_tuition] is a state that reflects the updated tuition and
 * corresponding happiness changes.
 * requires:
 *    - n is the updated tuition
 *    - st is the original, valid state *)
let do_tuition n st =
  {
    st with tuition = n;
            happiness = let updated_happiness =
                          st.happiness - ((n - st.tuition)/100) in
              if updated_happiness < (-100) then -100
              else if updated_happiness > 100 then 100
              else updated_happiness
  }

(* [do_time st] is [st'] after a month has passed. *)
let do_time st =
  let disaster = gen_disaster (st.happiness*st.time_passed) in
  let happ = if disaster <> None then st.happiness - 10 else st.happiness in
  let grid = update_grid happ st.grid in
  let pop = get_num grid get_rpop in
  let money = if (st.time_passed + 1) mod 12 <> 0
    then st.money - (get_num st.grid get_rmain)
    else st.money + pop*st.tuition in
  let lose = (money < 0 || pop <= 0) && st.time_passed > 11 in
  let message = if lose then Some "You Lost." else if disaster <> None then
      Some "A natural disaster occurred!" else None in (* more specfic message? *)
  {
    disaster = disaster;
    lose = lose;
    message = message;
    money = money;
    tuition = st.tuition;
    happiness = happ;
    time_passed = st.time_passed + 1;
    grid = grid;
  }

let do' (c:Command.command) st =
  match c with
  | Build (x,y,str) -> do_build x y str st
  | Delete (x,y) -> do_delete x y st
  | SetTuition n -> do_tuition n st
  | TimeStep -> do_time st
