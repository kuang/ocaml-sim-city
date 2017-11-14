(* open State *)
type building_type = Unbuildable | Dorm | Resource | Connection | Section
type command =
  | BUILD of int*int*building_type (**)
  | DELETE of int*int
  | UPDATE of int
  | TIME_STEP




val parse : Yojson.Basic.json -> command
