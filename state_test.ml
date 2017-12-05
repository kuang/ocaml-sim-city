open OUnit2
open State

let st = match (init_from_file "map.txt") with
  | Some x -> x
  | None -> init_state 20

let tests =
  [
    "init_happiness" >:: (fun _ -> assert_equal 50 (st |> get_happiness));
    "init_money" >:: (fun _ -> assert_equal 10000 (st |> get_money));
    "init_tuition" >:: (fun _ -> assert_equal 20 (st |> get_tuition));
    "init_time_passed" >:: (fun _ -> assert_equal 0 (st |> get_time_passed));
    "init_disaster" >:: (fun _ -> assert_equal None (st |> get_disaster));
    "init_message" >:: (fun _ -> assert_equal (Some "Welcome to the game!") (st |> get_message));
    "init_lose" >:: (fun _ -> assert_equal false (st |> get_lose));


    (* "init_money" >:: (fun _ -> assert_equal Some "welcome to the game!" (st |> get_money)); *)

  ]

let suite =
  "Adventure test suite"
  >::: tests

let _ = run_test_tt_main suite
