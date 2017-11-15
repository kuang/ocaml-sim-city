type command =
  | BUILD of int*int*State.building_type
  | DELETE of int*int
  | UPDATE of int
  | TIME_STEP

val parse : Yojson.Basic.json -> (command*State.gamestate) option
val unparse : State.gamestate -> Yojson.Basic.json
