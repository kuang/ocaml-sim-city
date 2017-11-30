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
let dorm_bcost = 700
let dining_bcost = 7000
let lecture_bcost = 7000
let power_bcost = 7000
let section_bcost = 0
let empty_bcost = 0

(* global delete costs *)
let road_dcost = 10
let pline_dcost = 10
let dorm_dcost = 500
let dining_dcost = 500
let lecture_dcost = 500
let power_dcost = 500
let section_dcost = 0
let empty_dcost = 0

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

(* wrapper function to easily access deletion costs of a given building type *)
let get_dcost (b:building_type) = match b with
  | Dorm _ -> dorm_dcost
  | Dining _ -> dining_dcost
  | Lecture _ -> lecture_dcost
  | Power _ -> power_dcost
  | Road -> road_dcost
  | Pline -> pline_dcost
  | Section _ -> section_dcost
  | Empty -> empty_dcost

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

(* [propagate_resource g xs c f] applies [f] to every square in [g] which is
 * connected to a square in [xs] by a series of connectors of type [c];
 * helper function for [update_resources] *)
let propagate_resource g xs c f =
  let adjacents x y = [x - 1, y; x + 1, y; x, y - 1; x, y + 1] in
  let visited = Array.(make_matrix (length g) (length g.(0)) false) in
  let rec aux (x, y) =
    g.(x).(y) <- f g.(x).(y);
    visited.(x).(y) <- true;
    adjacents x y |> List.iter ( fun (x', y') ->
      if   g.(x').(y').btype = c
      then (
        if    not visited.(x').(y')
        then  aux (x', y') )
      else g.(x').(y') <- f g.(x').(y') ) in
  List.iter aux xs

(* [update_resources g] updates [g] to have every square accurately reflect its
 * resource access. *)
let update_resources (g : square array array) : unit =
  let (d, l, p) = ref [], ref [], ref [] in
  for x = 0 to Array.length g - 1 do
    for y = 0 to Array.length g.(x) - 1 do
      g.(x).(y) <- { g.(x).(y) with
        dining_access = false;
        lec_access = false;
        power_access = false
      };
      match g.(x).(y).btype with
      | Dining  _ -> d := (x, y) :: !d
      | Lecture _ -> l := (x, y) :: !l
      | Power   _ -> p := (x, y) :: !p
      | _         -> ()
    done
  done;
  propagate_resource g !d Road  (fun s -> {s with dining_access = true});
  propagate_resource g !l Road  (fun s -> {s with lec_access = true});
  propagate_resource g !p Pline (fun s -> {s with power_access = true})
  
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

(*Helper for place_building- generates the surrounding 8 Section buildings*)
and place_sections x1 y1 ((x2,y2):int*int) st =
  let st1 = place_building (x1-1) (y1-1) (Section(x2,y2)) st in
  let st2 = place_building (x1-1) (y1) (Section(x2,y2)) st1 in
  let st3 = place_building (x1-1) (y1+1) (Section(x2,y2)) st2 in
  let st4 = place_building (x1) (y1-1) (Section(x2,y2)) st3 in
  let st5 = place_building (x1) (y1+1) (Section(x2,y2)) st4 in
  let st6 = place_building (x1+1) (y1-1) (Section(x2,y2)) st5 in
  let st7 = place_building (x1+1) (y1) (Section(x2,y2)) st6 in
  place_building (x1+1) (y1+1) (Section(x2,y2)) st7


(*Main logic for building something new. If invalid location, returns [st] but
 *with message field "Invalid build location." If not enough money, returns
 *[st] but with message field "Invalid funds." *)
and do_build x y (b:building_type) st : gamestate =
  let moneycheck_state = update_state_money b st in
  match moneycheck_state.message with
  | Some "Invalid funds." -> moneycheck_state
  | _ ->
    let placed_building_st = (
    match b with
      | Dorm _ | Dining _ | Lecture _ | Power _ ->
      let max_input = ((Array.length st.grid)-2) in
      if (x<1 || x>max_input || y<1 || y>max_input) then
        {
          st with
          message = Some "Invalid build location.";
        }
      else place_building x y b moneycheck_state
      | _ ->
        let max_input = ((Array.length st.grid)-1) in
        if (x<0 || x>max_input || y<0 || y>max_input) then
          {
            st with
            message = Some "Invalid build location.";
          }
        else place_building x y b moneycheck_state) in
    match placed_building_st.message  with
    | Some "Invalid build location." -> placed_building_st
    | _ ->
      {
        placed_building_st with
        time_passed = placed_building_st.time_passed+1;
      }
and update_state_money b st =
  let bcost = get_bcost b in
  if bcost<st.money then
    {
      st with
      money = (st.money-bcost);
      message = None;
    }
  else
    {
      st with
      message = Some "Invalid funds.";
    }

(* returns: Updated square "reset" to an empty square with no buildings on it.
 * requires: [sq] is a valid square. *)
let delete_square sq = {sq with
                        btype = Empty;
                        level = 0;
                        maintenance_cost = 0;
                        population = 0}

(* returns: Updated state with building on [sq] deleted
 * requires:
 *  - [x] is a valid grid x-coordinate
 *  - [y] is a valid grid y-coordinate
 *  - [st] is the current, valid state
 *  - [sq] is the square at (x,y) *)
let delete_square_grid x y st sq =
  let _ = st.grid.(x).(y) <- delete_square sq in st

(* returns: Updated state with building and sections deleted corresponding
 * to building at grid coordinates (x,y). *)
let delete_b_sections x y st =
  let st1 = delete_square_grid (x-1) (y-1) st st.grid.(x-1).(y-1) in
  let st2 = delete_square_grid (x-1) y st1 st1.grid.(x-1).(y) in
  let st3 = delete_square_grid (x-1) (y+1) st2 st2.grid.(x-1).(y+1) in
  let st4 = delete_square_grid x (y-1) st3 st3.grid.(x).(y-1) in
  let st5 = delete_square_grid x y st4 st4.grid.(x).(y) in
  let st6 = delete_square_grid x (y+1) st5 st5.grid.(x).(y+1) in
  let st7 = delete_square_grid (x+1) (y-1) st6 st6.grid.(x+1).(y-1) in
  let st8 = delete_square_grid (x+1) y st7 st7.grid.(x+1).(y) in
  delete_square_grid (x+1) (y+1) st8 st8.grid.(x+1).(y+1)

(* returns: [delete_building] is an updated state with building on [sq]
 * deleted if there is enough money to delete the building.
 * requires:
 *  - [x] is a valid grid x-coordinate
 *  - [y] is a valid grid y-coordinate
 *  - [st] is the current, valid state
 *  - [sq] is the square at (x,y) *)
let delete_building x y st sq =
  if st.money - (get_dcost sq.btype) > 0 then
    match sq.btype with
    | Dorm _ | Dining _ | Lecture _ | Power _ -> delete_b_sections x y st
    | Road | Pline -> delete_square_grid x y st sq
    | Section _ | Empty -> st
  else {st with message = Some "You don't have enough money for that."}

(* returns: [do_delete] is an updated state with building on the square at
 * grid coordinates (x,y) deleted if one exists, and there is enough money
 * to cover demolition costs.
 * requires:
 *  - [x] is a valid grid x-coordinate
 *  - [y] is a valid grid y-coordinate
 *  - [st] is the current, valid state *)
let rec do_delete x y st =
  let cur_square = st.grid.(x).(y) in
  match cur_square.btype with
  | Dorm _ | Dining _ | Lecture _ | Power _ | Road | Pline ->
    delete_building x y st cur_square
  | Section (x,y) -> do_delete x y st
  | Empty -> {st with message = Some "Nothing to delete here."}

(* returns: [do_tuition] is a state that reflects the updated tuition and
 * corresponding happiness changes.
 * requires:
 *    - [n] is the updated tuition
 *    - [st] is the original, valid state *)
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

(* returns: [valid_delete_coord] is an updated state with building on the
 * square at grid coordinates (x,y) deleted if one exists, grid coordinates
 * (x,y) are valid grid coordinates, and there is enough money to cover
 * demolition costs. Otherwise returns the same state with an invalid delete
 * location message.
 * requires:
 *  - [x] is a grid coordinate
 *  - [y] is a grid coordinate
 *  - [st] is the current, valid state *)
let valid_delete_coord x y st =
  if x >= 0 && x < Array.length st.grid &&
     y >= 0 && y < Array.length st.grid then
    do_delete x y st
  else
    {st with message = Some "Invalid delete location."}

let do' (c:command) st =
  match c with
  | Build (x,y,b) -> do_build x y b st
  | Delete (x,y) -> valid_delete_coord x y st
  | SetTuition n -> do_tuition n st
  | TimeStep -> do_time st

