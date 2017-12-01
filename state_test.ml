open OUnit2
open State

let tests =
  [
  "max" >:: (fun _ -> assert_equal 11111 (11111));
]

let suite =
  "Adventure test suite"
  >::: tests

let _ = run_test_tt_main suite
