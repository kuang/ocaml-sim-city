(* Represents commands to be executed on the state. Build, Delete, and
 * SetTuition are commands issued by the user to build or destroy a building at
 * specific coordinates, or to change the university's tuition rate. TimeStep
 * is a command issued automatically at regular time intervals that allows time
 * to pass in the game state. *)
 (* type command =
   | Build of int*int*string
   | Delete of int*int
   | SetTuition of int
   | TimeStep *)

(* Parses a JSON object from an HTTP request into OCaml values. Returns None if
 * the input is not a valid representation of a command and a gamestate. *)
(* val parse : Yojson.Basic.json -> (command*State.gamestate) option *)

(* Converts a gamestate into its JSON representation. *)
(* val unparse : State.gamestate -> Yojson.Basic.json *)
