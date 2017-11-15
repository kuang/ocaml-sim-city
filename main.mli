(* The main loop of the server. Takes HTTP requests with a gamestate and a
 * command formatted in JSON and responds with an updated gamestate serialized
 * to JSON. *)
val main : unit -> unit
