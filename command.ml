(* open Yojson.Basic *)
(* open Yojson.Basic.Util *)
open State

(* Represents commands to be executed on the state. Build, Delete, and
 * SetTuition are commands issued by the user to build or destroy a building at
 * specific coordinates, or to change the university's tuition rate. TimeStep
 * is a command issued automatically at regular time intervals that allows time
 * to pass in     the game state. *)
type command =
  | Build of int*int*State.building_type
  | Delete of int*int
  | SetTuition of int
  | TimeStep
