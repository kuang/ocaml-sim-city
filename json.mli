
(* [load_state fn] reads from the file named [fn] and returns a parsed gamstate,
 * or None if [fn] is not a valid JSON state encoding. *)
val load_state : string -> State.gamestate option

(* [save_state fn st] saves [st] as JSON to the file named [fn], returns whether
 * it was successful. *)
val save_state : string -> State.gamestate -> bool
