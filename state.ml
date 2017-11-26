type terrain = Water | Forest | Clear | Gorges

type disaster = Fire | Blizzard | Prelim

(*global maintenance costs*)
let road_mcost = 10
let pline_mcost = 10
let dorm_mcost = 300
let dining_mcost = 300
let lecture_mcost = 300
let power_mcost = 300
let section_mcost = 0
let empty_mcost = 0

(*global build costs*)
let road_bcost = 50
let pline_bcost = 50
let dorm_bcost = 7000
let dining_bcost = 7000
let lecture_bcost = 7000
let power_bcost = 7000
let section_bcost = 0
let empty_bcost = 0

(*dorm initial population*)
let dorm_init_pop = 20

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

(*wrapper function to easily access maintenance costs of a given building type*)
let get_mcost (b:building_type) = match b with
  | Dorm _ -> dorm_mcost
  | Dining _ -> dining_mcost
  | Lecture _ -> lecture_mcost
  | Power _ -> power_mcost
  | Road -> road_mcost
  | Pline -> pline_mcost
  | Section _ -> section_mcost
  | Empty -> empty_mcost

(*wrapper function to easily access building costs of a given building type*)
let get_bcost (b:building_type) = match b with
  | Dorm _ -> dorm_bcost
  | Dining _ -> dining_bcost
  | Lecture _ -> lecture_bcost
  | Power _ -> power_bcost
  | Road -> road_bcost
  | Pline -> pline_bcost
  | Section _ -> section_bcost
  | Empty -> empty_bcost

(* Represents commands to be executed on the state. Build, Delete, and
 * SetTuition are commands issued by the user to build or destroy a building at
 * specific coordinates, or to change the university's tuition rate. TimeStep
 * is a command issued automatically at regular time intervals that allows time
 * to pass in     the game state. *)
type command =
  | Build of int*int*building_type
  | Delete of int*int
  | SetTuition of int
  | TimeStep

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
  money = 2000;
  tuition = 5;
  happiness = 50;
  time_passed = 0;
  grid = Array.make_matrix grid_size grid_size init_square;
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

(*This following code literally only places a building on the specific
  square, doesn't implement any resource connection stuff*)
let rec place_building (x:int) (y:int) (b:building_type) st : gamestate =
  let curr_square = st.grid.(x).(y) in
  match b with
  | Lecture _ | Power _ | Dining _ -> (*no population, multiple squares*)
    let new_square = {
          curr_square with
          btype = b;
          level = 1;
          maintenance_cost = get_mcost b;
          population = 0;
        } in let _ =  st.grid.(x).(y) <- new_square in
      place_sections x y (x,y) st
  | Dorm  _ -> (*has population, multiple squares*)
    let new_square = {
          curr_square with
          btype = b;
          level = 1;
          maintenance_cost = dorm_mcost;
          population = dorm_init_pop;
        } in let _ =  st.grid.(x).(y) <- new_square in
      place_sections x y (x,y) st
  | Road | Pline | Section _ | Empty -> (*single square, no pop*)
    let new_square = {
        curr_square with
        btype = b;
        level = 1;
        maintenance_cost = get_mcost b;
        population = 0;
      } in let _ =  st.grid.(x).(y) <- new_square in st

(*Helper for place_building- generates the surrouding 8 Section buildings*)
and place_sections x1 y1 ((x2,y2):int*int) st =
  let st1 = place_building (x1-1) (y1-1) (Section(x2,y2)) st in
  let st2 = place_building (x1-1) (y1) (Section(x2,y2)) st1 in
  let st3 = place_building (x1-1) (y1+1) (Section(x2,y2)) st2 in
  let st4 = place_building (x1) (y1-1) (Section(x2,y2)) st3 in
  let st5 = place_building (x1) (y1+1) (Section(x2,y2)) st4 in
  let st6 = place_building (x1+1) (y1-1) (Section(x2,y2)) st5 in
  let st7 = place_building (x1+1) (y1) (Section(x2,y2)) st6 in
  place_building (x1+1) (y1+1) (Section(x2,y2)) st7

and do_build x y (b:building_type) st : gamestate =
  let place_building_state = (
  match b with
    | Dorm _ | Dining _ | Lecture _ | Power _ ->
    let max_input = ((Array.length st.grid)-2) in
    if (x<1 || x>max_input || y<1 || y>max_input) then
       {
        st with
        message = Some "Invalid build location.";
       }
       else place_building x y b st
    | _ ->
      let max_input = ((Array.length st.grid)-1) in
      if (x<0 || x>max_input || y<0 || y>max_input) then
        {
          st with
          message = Some "Invalid build location.";
        }
      else place_building x y b st) in
  match place_building_state.message with
  | Some "Invalid build location." -> place_building_state
  | _ -> update_state_money b place_building_state

and update_state_money b st =
  {
    st with
    money = (st.money- (get_bcost b));
  }


let do_delete x y st =
  let cur_square = st.grid.(x).(y) in
  match cur_square.btype with
  | Dorm lst -> st
  | Resource rs -> st
  | Road -> st
  | Pline -> st
  | Section (x,y) -> st
  | Empty -> st (* No building to delete *)

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

let do' (c:command) st =
  match c with
  | Build (x,y,b) -> do_build x y b st
  | Delete (x,y) -> do_delete x y st
  | SetTuition n -> do_tuition n st
  | TimeStep -> do_time st
