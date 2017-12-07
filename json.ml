open State

let (>>) f g x = g (f x)

let load_state fn = Yojson.Basic.Util.( try
  let j = Yojson.Basic.from_file fn in
  Some {
    disaster = (
      match member "disaster" j with
      | `Null -> None
      | `String "Fire" -> Some Fire
      | `String "Blizzard" -> Some Blizzard
      | `String "Prelim" -> Some Prelim
      | _ -> failwith "Failed state parse");
    lose = member "lose" j |> to_bool;
    message = member "message" j |> to_string_option;
    money = member "money" j |> to_int;
    tuition = member "tuition" j |> to_int;
    happiness = member "happiness" j |> to_int;
    time_passed = member "time_passed" j |> to_int;
    grid = member "grid" j |> to_list |> List.map (
      to_list >> List.map (fun s ->
      {
        btype = (
          match member "btype" s with
          | `String "Dorm" -> Dorm
          | `String "Dining" -> Dining
          | `String "Lecture" -> Lecture
          | `String "Power" -> Power
          | `String "Park" -> Park
          | `String "Road" -> Road
          | `String "Pline" -> Pline
          | `String "Empty" -> Empty
          | `List [`Int x; `Int y] -> Section (x, y)
          | _ -> failwith "Failed state parse");
        level = member "level" s |> to_int;
        maintenance_cost = member "maintenance_cost" s |> to_int;
        population = member "population" s |> to_int;
        terrain = (
          match member "terrain" s |> to_string with
          | "Water" -> Water
          | "Forest" -> Forest
          | "Clear" -> Clear
          | _ -> failwith "Failed state parse");
        dining_access = member "dining_access" s |> to_bool;
        lec_access = member "lec_access" s |> to_bool;
        power_access = member "power_access" s |> to_bool
      }) >> Array.of_list) |> Array.of_list
  }
  with _ -> None )

let save_state fn st = Yojson.Basic.(
  let j = `Assoc [
    "disaster", (
      match st.disaster with
      | None -> `Null
      | Some Fire -> `String "Fire"
      | Some Blizzard -> `String "Blizzard"
      | Some Prelim -> `String "Prelim");
    "lose", `Bool st.lose;
    "message", (
      match st.message with
      | None -> `Null
      | Some s -> `String s);
    "money", `Int st.money;
    "tuition", `Int st.tuition;
    "happiness", `Int st.happiness;
    "time_passed", `Int st.time_passed;
    "grid", `List (st.grid |> Array.to_list |> List.map (fun r ->
      `List (Array.to_list r |> List.map (fun s ->
        `Assoc [
          "btype", (
            match s.btype with
            | Dorm -> `String "Dorm"
            | Dining -> `String "Dining"
            | Lecture -> `String "Lecture"
            | Power -> `String "Power"
            | Park -> `String "Park"
            | Road -> `String "Road"
            | Pline -> `String "Pline"
            | Empty -> `String "Empty"
            | Section (x, y) -> `List [`Int x; `Int y]);
          "level", `Int s.level;
          "maintenance_cost", `Int s.maintenance_cost;
          "population", `Int s.population;
          "terrain", `String (
            match s.terrain with
            | Water -> "Water"
            | Forest -> "Forest"
            | Clear -> "Clear");
          "dining_access", `Bool s.dining_access;
          "lec_access", `Bool s.lec_access;
          "power_access", `Bool s.power_access
        ]))))
    ] in
  try to_file fn j; true
  with _ -> false )
