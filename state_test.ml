open OUnit2
open State

let st = match (init_from_file "map.txt") with
  | Some x -> x
  | None -> init_state 20

(*first "building" test- create a dorm at (1,1)*)
let st1 = do' (Build (1,1,Dorm)) st

let tests =
  [
    "init_happiness" >:: (fun _ -> assert_equal 50 (st |> get_happiness));
    "init_money" >:: (fun _ -> assert_equal 100000 (st |> get_money));
    "init_tuition" >:: (fun _ -> assert_equal 20000 (st |> get_tuition));
    (* "init_time_passed" >:: (fun _ -> assert_equal "0" (st |> get_time_passed)); *)
    "init_disaster" >:: (fun _ -> assert_equal None (st |> get_disaster));
    "init_message" >:: (fun _ -> assert_equal (Some "Welcome to the game!") (st |> get_message));
    "init_lose" >:: (fun _ -> assert_equal false (st |> get_lose));


    (*test suite for first dorm built- represented by [st1]*)
    "st1_happiness" >:: (fun _ -> assert_equal 50 (st1 |> get_happiness));
    "st1_money">::
    (fun _ -> assert_equal (100000 -(get_bcost Dorm)) (st1 |> get_money));
    "st1_tuition" >:: (fun _ -> assert_equal 20000 (st1 |> get_tuition));
    (* "init_time_passed" >:: (fun _ -> assert_equal "0" (st |> get_time_passed)); *)
    "st1_disaster" >:: (fun _ -> assert_equal None (st |> get_disaster));
    "st1_message" >:: (fun _ -> assert_equal (Some "Welcome to the game!") (st |> get_message));
    "st1_lose" >:: (fun _ -> assert_equal false (st |> get_lose));
  ]

let suite =
  "Adventure test suite"
  >::: tests

let _ = run_test_tt_main suite
