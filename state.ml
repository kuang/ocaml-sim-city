type terrain = Water | Forest | Clear

type disaster = Fire | Blizzard | Prelim

(*global maintenance costs*)
let dorm_mcost = 300
let dining_mcost = 300
let lecture_mcost = 300
let power_mcost = 300
let park_mcost = 10
let road_mcost = 10
let pline_mcost = 10
let section_mcost = 0
let empty_mcost = 0


(*global build costs*)
let dorm_bcost = 10000
let dining_bcost = 10000
let lecture_bcost = 10000
let power_bcost = 500
let park_bcost = 750
let road_bcost = 50
let pline_bcost = 50
let section_bcost = 0
let empty_bcost = 0

(* global delete costs *)
let dorm_dcost = 500
let dining_dcost = 500
let lecture_dcost = 500
let power_dcost = 500
let park_dcost = 500
let road_dcost = 10
let pline_dcost = 10
let section_dcost = 500
let empty_dcost = 0

(*dorm initial population*)
let dorm_init_pop = 20

(*park happiness*)
let park_happiness = 25
let forest_happiness = 5

(* happiness deduction for natural disasters *)
let fire_happiness = 5
let blizzard_happiness = 15
let prelim_happiness = 10

(* state initial settings *)
let init_money = 100000
let init_tuition = 20000

type building_type =
  | Dorm
  | Dining
  | Lecture
  | Power
  | Park
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
  message : string option;
  money : int;
  tuition : int;
  happiness: int;
  time_passed : int;
  grid : square array array
}

(*wrapper function to easily access maintenance costs of a given building type*)
let get_mcost (b:building_type) = match b with
  | Dorm  -> dorm_mcost
  | Dining  -> dining_mcost
  | Lecture -> lecture_mcost
  | Power -> power_mcost
  | Park -> park_mcost
  | Road -> road_mcost
  | Pline -> pline_mcost
  | Section _ -> section_mcost
  | Empty -> empty_mcost

(*wrapper function to easily access building costs of a given building type*)
let get_bcost (b:building_type) = match b with
  | Dorm -> dorm_bcost
  | Dining -> dining_bcost
  | Lecture -> lecture_bcost
  | Power -> power_bcost
  | Road -> road_bcost
  | Park -> park_bcost
  | Pline -> pline_bcost
  | Section _ -> section_bcost
  | Empty -> empty_bcost

(* wrapper function to easily access deletion costs of a given building type *)
let get_dcost (b:building_type) = match b with
  | Dorm -> dorm_dcost
  | Dining -> dining_dcost
  | Lecture -> lecture_dcost
  | Power -> power_dcost
  | Park -> park_dcost
  | Road -> road_dcost
  | Pline -> pline_dcost
  | Section _ -> section_dcost
  | Empty -> empty_dcost

(* Represents commands to be executed on the state. Build, Delete, and
 * SetTuition are commands issued by the user to build or destroy a building at
 * specific coordinates, or to change the university's tuition rate. TimeStep
 * is a command issued automatically at regular time intervals that allows time
 * to pass in the game state. *)
type command =
  | Build of int*int*building_type
  | Delete of int*int
  | SetTuition of int
  | TimeStep

(*creates an initial square.*)
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

(*creates an initial gamestate. *)
let init_state (grid_size:int)= {
  disaster = None;
  lose = false;
  message = Some ("Welcome to the game!");
  money = init_money;
  tuition = init_tuition;
  happiness = 50;
  time_passed = 0;
  grid = Array.make_matrix grid_size grid_size init_square;
}

let init_from_file (filename : string) =
  let rec lines c =
    try
      let l = input_line c in
      l :: lines c
    with End_of_file -> [] in
  let rec list_of_string = function
    | "" -> []
    | st -> String.(
      get st 0 ::
      list_of_string (sub st 1 (length st - 1))) in
  let square_of_char = function
    | '_' -> init_square
    | '~' -> { init_square with terrain = Water }
    | 'T' -> { init_square with terrain = Forest }
    | _   -> raise (Invalid_argument "Unregonized character") in
(*  try *)
    match open_in filename |> lines with
    | [] -> None
    | h :: t as xs ->
      let len = String.length h in
      if List.exists (fun l -> String.length l <> len) t
      then None
      else Some {
        disaster = None;
        lose = false;
        message = Some ("Welcome to the game!");
        money = init_money;
        tuition = init_tuition;
        happiness = 50;
        time_passed = 0;
        grid =
          List.map (fun l ->
            list_of_string l |>
            List.map square_of_char |>
            Array.of_list) xs |>
          Array.of_list
      }
(*  with _ -> None *)



(* returns: [month] is the current month calculated from time passed (number
 * of turns so far).
 * requires: turns is the number of turns so far. *)
let month turns =
  match (turns + 3) mod 12 with
  | 0 -> "Jan "
  | 1 -> "Feb "
  | 2 -> "Mar "
  | 3 -> "Apr "
  | 4 -> "May "
  | 5 -> "Jun "
  | 6 -> "Jul "
  | 7 -> "Aug "
  | 8 -> "Sep "
  | 9 -> "Oct "
  | 10 -> "Nov "
  | _ -> "Dec "

(* returns: [year] is the current year calculated from time passed (number of
 * turns so far).
 * requires: turns is the number of turns so far. *)
let year turns =
  string_of_int (((turns + 3) / 12) + 1865)


(*returns time_passed of [st] as a string.*)
let get_time_passed (st:gamestate)  =
  month st.time_passed ^ year st.time_passed

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

(* [get_parks row] is the number of parks in [row] *)
let get_parks row =
  Array.fold_left (fun acc b  -> acc + if b.btype = Park then 1 else 0) 0 row

(* [get_rmain row] is the total maintenance cost of all squares in [row] *)
let get_rmain row =
  Array.fold_left (fun (acc:int) b -> b.maintenance_cost + acc) 0 row

(* [get_num st] is the sum of [f x] over all squares [x] in [grid] *)
let get_num grid f : int =
  Array.fold_left (fun (acc:int) r -> f r + acc) 0 grid

(* [update_build happ b] is [b'], where [b'] is [b] after a month with
 * happiness level [happ]. *)
let update_build st happ (b : square) =
  match b.btype with
  | Dorm -> begin
      let newpop = max 0
          (if (b.dining_access && b.lec_access && b.power_access) then
    b.population + (b.level)*happ else 0)
  in {
    b with
    population = newpop;
    }
    end
  | _ -> b

(* [update_row happ r] is [r'] containing squares which have stepped one
 * month under happiness level [happ]. *)
let update_row st happ r =
  Array.map (update_build st happ) r

(* [update_grid happ grid] is [grid'] containing squares which have stepped one
 * month under happiness level [happ]. *)
let update_grid st happ (grid:square array array) =
  Array.map (update_row st happ) grid

let three_by_three x y =
  [ x - 1, y - 1; x, y - 1; x + 1, y - 1 ;
    x - 1, y    ; x, y    ; x + 1, y     ;
    x - 1, y + 1; x, y + 1; x + 1, y + 1 ]

(* [propagate_resource g xs c f] applies [f] to every square in [g] which is
 * connected to a square in [xs] by a series of connectors of type [c];
 * helper function for [update_resources] *)
let propagate_resource g xs c f =
  let grid_size = Array.length g in
  let in_bounds x y = 0 <= x && x < grid_size && 0 <= y && y < grid_size in
  let adjacents x y = [x - 1, y; x + 1, y; x, y - 1; x, y + 1] in
  let set_building x y = three_by_three x y |>
    List.iter (fun (x', y') -> g.(x').(y') <- f g.(x').(y')) in
  let visited = Array.make_matrix grid_size grid_size false in
  let rec aux (x, y) =
    g.(x).(y) <- f g.(x).(y);
    visited.(x).(y) <- true;
    adjacents x y |> List.iter ( fun (x', y') ->
      if   in_bounds x' y'
      then match g.(x').(y').btype with
           | x when x = c ->
               if   not visited.(x').(y')
               then aux (x', y')
           | Dorm | Dining | Power | Lecture | Park ->
               set_building x' y'
           | Section (x'', y'') ->
               set_building x'' y''
           | _ -> () ) in
  List.iter aux xs

(* [update_resources g] updates [g] to have every square accurately reflect its
 * resource access. *)
let update_resources (g : square array array) =
  let (d, l, p) = ref [], ref [], ref [] in
  for x = 0 to Array.length g - 1 do
    for y = 0 to Array.length g.(x) - 1 do
      g.(x).(y) <- { g.(x).(y) with
        dining_access = false;
        lec_access = false;
        power_access = false
      };
      match g.(x).(y).btype with
      | Dining  -> d := three_by_three x y @ !d
      | Lecture -> l := three_by_three x y @ !l
      | Power   -> p := three_by_three x y @ !p
      | _       -> ()
    done
  done;
  propagate_resource g !d Road  (fun s -> {s with dining_access = true});
  propagate_resource g !l Road  (fun s -> {s with lec_access = true});
  propagate_resource g !p Pline (fun s -> {s with power_access = true});
  g

(*This following code literally only places a building on the specific
  square, doesn't implement any resource connection stuff*)
let rec place_building (x:int) (y:int) (b:building_type) st : gamestate =
  let curr_square = st.grid.(x).(y) in
  match b with
  | Lecture  | Power  | Dining | Park -> (*no population, multiple squares*)
    (match curr_square.terrain with
     | Forest ->
       let new_square =
         {
           curr_square with
           btype = b;
           level = 1;
           terrain = Clear;
           maintenance_cost = get_mcost b;
           population = 0;
         }
       in let _ =  st.grid.(x).(y) <- new_square in
       let placed_building_st = place_sections x y (x,y) st in
       {
         placed_building_st with
         happiness = placed_building_st.happiness - forest_happiness;
       }
      | _ ->
      let new_square =
        {
          curr_square with
          btype = b;
          level = 1;
          maintenance_cost = get_mcost b;
          population = 0;
        }
    in let _ =  st.grid.(x).(y) <- new_square in place_sections x y (x,y) st)
  | Dorm  -> (*has population, multiple squares*)
    (match curr_square.terrain with
     | Forest ->
       let new_square =
         {
           curr_square with
           btype = b;
           level = 1;
           terrain = Clear;
           maintenance_cost = get_mcost b;
           population = 0;
         }
       in let _ =  st.grid.(x).(y) <- new_square in
       let placed_building_st = place_sections x y (x,y) st in
       {
         placed_building_st with
         happiness = placed_building_st.happiness - forest_happiness;
       }
     | _ ->
       let new_square =
         {
           curr_square with
           btype = b;
           level = 1;
           maintenance_cost = get_mcost b;
           population = 0;
         }
       in let _ =  st.grid.(x).(y) <- new_square in place_sections x y (x,y) st)
  | Road | Pline | Section _ | Empty -> (*single square, no pop*)
    (match curr_square.terrain with
     | Forest ->
       let new_square =
         {
           curr_square with
           btype = b;
           level = 1;
           terrain = Clear;
           maintenance_cost = get_mcost b;
           population = 0;
         }
       in let _ =  st.grid.(x).(y) <- new_square in
       {
         st with
         happiness = st.happiness - forest_happiness;
       }
     | _ ->
      let new_square = {
         curr_square with
         btype = b;
         level = 1;
         maintenance_cost = get_mcost b;
         population = 0;
       } in let _ =  st.grid.(x).(y) <- new_square in st)


(*Helper for place_building- generates the surrounding 8 Section buildings*)
and place_sections x1 y1 ((x2,y2):int*int) st : gamestate =
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
 *[st] but with message field "Insufficient funds." *)
and do_build x y (b:building_type) st : gamestate =
  let moneycheck_state = update_state_money b st in
  match moneycheck_state.message with
  | Some "Insufficient funds." -> moneycheck_state
  | _ ->
    let placed_building_st = (
      if(is_valid_location x y st b) then
        place_building x y b moneycheck_state
      else
          {
            st with
            message = Some "Invalid build location.";
          }) in
    match placed_building_st.message  with
    | Some "Invalid build location." -> placed_building_st
    | _ -> let happiness_update_st = (match b with
        | Park ->
          {placed_building_st with
           happiness = park_happiness+placed_building_st.happiness}
        | _ -> placed_building_st) in
      {
        happiness_update_st with
        grid = update_resources happiness_update_st.grid
      }

and update_state_money b st =
  let bcost = get_bcost b in
  if bcost<=st.money then
    {
      st with
      money = (st.money-bcost);
      message = None;
    }
  else
    {
      st with
      message = Some "Insufficient funds.";
    }

(*true if st.(x).(y) is a valid build location for a building of type b.*)
and is_valid_location (x:int) (y:int)(st:gamestate) (b:building_type) : bool =
  match b with
  | Dorm  | Dining  | Lecture  | Power | Park -> check_3x3 x y st
  | _ -> check_1x1 x y b st

(*true if st.(x).(y) is a valid build location for a 3x3 building.*)
and check_3x3 x y st : bool =
  let max_input = ((Array.length st.grid)-2) in
  if (x<1 || x>max_input || y<1 || y>max_input) then false
  else
    let g1 = st.grid.(x-1).(y-1) in
    let g2 = st.grid.(x-1).(y) in
    let g3 = st.grid.(x-1).(y+1) in
    let g4 = st.grid.(x).(y-1) in
    let g5 = st.grid.(x).(y) in
    let g6 = st.grid.(x).(y+1) in
    let g7 = st.grid.(x+1).(y-1) in
    let g8 = st.grid.(x+1).(y) in
    let g9 = st.grid.(x+1).(y+1) in
    if g1.btype<>Empty || g1.terrain==Water then false
    else if g2.btype<>Empty || g2.terrain==Water then false
    else if g3.btype<>Empty || g3.terrain==Water then false
    else if g4.btype<>Empty || g4.terrain==Water then false
    else if g5.btype<>Empty || g5.terrain==Water then false
    else if g6.btype<>Empty || g6.terrain==Water then false
    else if g7.btype<>Empty || g7.terrain==Water then false
    else if g8.btype<>Empty || g8.terrain==Water then false
    else if g9.btype<>Empty || g9.terrain==Water then false
    else true

(*true if st.(x).(y) is a valid build location for a 1x1 building. Roads can be built on water, other structures cannot. *)
and check_1x1 x y b st : bool =
  let max_in = ((Array.length st.grid)-1) in
  if (x<0||x>max_in ||y<0|| y>max_in || st.grid.(x).(y).btype<>Empty) then false
  else
    match b with
    | Road | Pline | Empty-> true
    | Section _ -> st.grid.(x).(y).terrain<>Water
    | _ -> false (*should never happen*)

(* returns: Updated square "reset" to an empty square with no buildings on it.
 * requires: [sq] is a valid square. *)
let delete_square sq = {sq with
                        btype = Empty;
                        level = 0;
                        maintenance_cost = 0;
                        population = 0
                       }

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
  let st9 = delete_square_grid (x+1) (y+1) st8 st8.grid.(x+1).(y+1) in
  {st9 with message = None}

(* returns: [delete_building] is an updated state with building on [sq]
 * deleted if there is enough money to delete the building.
 * requires:
 *  - [x] is a valid grid x-coordinate
 *  - [y] is a valid grid y-coordinate
 *  - [st] is the current, valid state
 *  - [sq] is the square at (x,y) *)
let delete_building x y st sq =
  let delete_money = st.money - (get_dcost sq.btype) in
  if delete_money >= 0 then
    match sq.btype with
    | Dorm  | Dining  | Lecture  | Power ->
      let st' = delete_b_sections x y st in
      {st' with money = delete_money; grid = update_resources st'.grid}
    | Park -> let st' = delete_b_sections x y st in
      {st' with money = delete_money;
                happiness = st'.happiness - park_happiness}
    | Road | Pline -> let st' = delete_square_grid x y st sq in
      {st' with money = delete_money;
                grid = update_resources st'.grid; message = None}
    | Section _ | Empty -> {st with money = delete_money; message = None}
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
  | Dorm  | Dining | Lecture | Power | Road | Pline | Park ->
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
                          st.happiness - ((n - st.tuition)/5000) in
              if updated_happiness < (-100) then -100
              else if updated_happiness > 100 then 100
              else updated_happiness
  }

(* [do_time st] is [st'] after a month has passed. *)
let do_time st =
  let disaster = gen_disaster (st.happiness*st.time_passed) in
  let dishapp = match disaster with
    | Some Fire -> fire_happiness
    | Some Blizzard -> blizzard_happiness
    | Some Prelim -> prelim_happiness
    | None -> 0 in
  let parkshapp = get_num st.grid get_parks in
  let tuthapp = (st.tuition - 60000)/5000 in
  let happ = max (st.happiness - dishapp - tuthapp + parkshapp) (-100) in
  let grid = update_grid st happ st.grid in
  let pop = get_num grid get_rpop in
  let money = st.money - (get_num st.grid get_rmain) +
    if (st.time_passed + 1) mod 12 = 0
    then pop*st.tuition else 0 in
  let lose = (money < 0 || pop <= 0) && st.time_passed > 11 in
  let message = if lose then Some "You Lost."
    else match disaster with
      | Some Fire -> Some ("A fire occurred! Happiness decreased by "
                           ^ string_of_int fire_happiness)
      | Some Blizzard -> Some ("A blizzard occurred! Happiness decreased by "
                               ^ string_of_int blizzard_happiness)
      | Some Prelim -> Some ("A prelim occurred! Happiness decreased by "
                             ^ string_of_int prelim_happiness)
      | None -> None in
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

let rec do' (c:command) st =
  match c with
  | Build (x,y,b) -> do_build x y b st
  | Delete (x,y) -> valid_delete_coord x y st
  | SetTuition n -> do_tuition n st
  | TimeStep -> do_time st
