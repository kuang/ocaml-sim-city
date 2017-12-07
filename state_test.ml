open OUnit2
open State

let st = match (init_from_file "map.txt") with
  | Some x -> x
  | None -> init_state 20

let init_tests =
  [
    "init_happiness" >:: (fun _ -> assert_equal 50 (st.happiness));
    "init_money" >:: (fun _ -> assert_equal 100000 (st.money));
    "init_tuition" >:: (fun _ -> assert_equal 20000 (st.tuition));
    (* "init_time_passed" >:: (fun _ -> assert_equal "0" (st |> get_time_passed)); *)
    "init_disaster" >:: (fun _ -> assert_equal None (st.disaster));
    "init_message" >:: (fun _ -> assert_equal (Some "Welcome to the game!") (st.message));
    "init_lose" >:: (fun _ -> assert_equal false (st.lose));
  ]


let st1 = do' (Build (11,12,Dorm)) st
let dorm1 = st1.grid.(11).(12)

let dorm1_tests = [
    (*test suite for state after first dorm built- represented by [st1]*)
    "st1_happiness" >:: (fun _ -> assert_equal 50 (st1.happiness));
    "st1_money">::
    (fun _ -> assert_equal (100000 -(get_bcost Dorm)) (st1.money));
    "st1_tuition" >:: (fun _ -> assert_equal 20000 (st1.tuition));
    (* "init_time_passed" >:: (fun _ -> assert_equal "0" (st |> get_time_passed)); *)
    "st1_disaster" >:: (fun _ -> assert_equal None (st1.disaster));
    "st1_message" >::
    (fun _ -> assert_equal (Some "Welcome to the game!") (st.message));
    "st1_lose" >:: (fun _ -> assert_equal false (st.lose));

    (*test suite for dorm attributes*)
    "dorm1_btype" >:: (fun _ -> assert_equal Dorm (dorm1.btype));
    "dorm1_level" >:: (fun _ -> assert_equal 1 (dorm1.level));
    "dorm1_population" >:: (fun _ -> assert_equal 0 (dorm1.population));
    "dorm1_mcost">::
    (fun _ -> assert_equal (get_mcost Dorm) (dorm1.maintenance_cost));
    "dorm1_terrain" >:: (fun _ -> assert_equal Clear (dorm1.terrain));
    "dorm1_daccess" >:: (fun _ -> assert_equal false (dorm1.dining_access));
    "dorm1_laccess" >:: (fun _ -> assert_equal false (dorm1.lec_access));
    "dorm1_paccess" >:: (fun _ -> assert_equal false (dorm1.power_access));
  ]


let st2 = do' (Build (13,11,Road)) st1
let st3 = do' (Build (14,11,Road)) st2
let st4 = do' (Build (15,11,Road)) st3

let lroad1 = st2.grid.(13).(11)
let lroad2 = st3.grid.(14).(11)
let lroad3 = st4.grid.(15).(11)

let lroad_tests = [
    (*test left road connection*)
    "lroad1_btype" >:: (fun _ -> assert_equal Road (lroad1.btype));
    "lroad1_level" >:: (fun _ -> assert_equal 1 (lroad1.level));
    "lroad1_population" >:: (fun _ -> assert_equal 0 (lroad1.population));
    "lroad1_mcost">::
    (fun _ -> assert_equal (get_mcost Road) (lroad1.maintenance_cost));
    "lroad1_terrain" >:: (fun _ -> assert_equal Clear (lroad1.terrain));
    "lroad1_daccess" >:: (fun _ -> assert_equal false (lroad1.dining_access));
    "lroad1_laccess" >:: (fun _ -> assert_equal false (lroad1.lec_access));
    "lroad1_paccess" >:: (fun _ -> assert_equal false (lroad1.power_access));
    "st2_money">::
    (fun _ -> assert_equal (100000-(get_bcost Dorm)-(get_bcost Road)) (st2.money));


    (*Test that roads are allowed to be built over water*)
    "lroad2_terrain" >:: (fun _ -> assert_equal Water (lroad2.terrain));
    "lroad3_terrain" >:: (fun _ -> assert_equal Water (lroad2.terrain));

  ]
let st5 = do' (Build (17,10,Lecture)) st4
let lec1 = st5.grid.(17).(10)
let dorm1_new = st5.grid.(11).(12)

let lecture_tests = [
  (*check that lecture hall was build correctly*)
  "lec1_btype" >:: (fun _ -> assert_equal Lecture (lec1.btype));
  "lec1_level" >:: (fun _ -> assert_equal 1 (lec1.level));
  "lec1_population" >:: (fun _ -> assert_equal 0 (lec1.population));
  "lec1_mcost">::
  (fun _ -> assert_equal (get_mcost Lecture) (lec1.maintenance_cost));
  "lec1_terrain" >:: (fun _ -> assert_equal Clear (lec1.terrain));
  "lec1_daccess" >:: (fun _ -> assert_equal false (lec1.dining_access));
  "lec1_laccess" >:: (fun _ -> assert_equal true (lec1.lec_access));
  "lec1_paccess" >:: (fun _ -> assert_equal false (lec1.power_access));

  (*check that state attributes are changed correctly*)
  "st5_happiness" >:: (fun _ -> assert_equal 50 (st1.happiness));
  "st5_money">::
  (fun _ -> assert_equal
      (100000 -
       (get_bcost Dorm)-
       3*(get_bcost Road)-
       (get_bcost Lecture)) (st5.money));


  (*check that only lecture access is changed*)
  "d_new_daccess" >:: (fun _ -> assert_equal false (dorm1_new.dining_access));
  "d_new_laccess" >:: (fun _ -> assert_equal true (dorm1_new.lec_access));
  "d_new_paccess" >:: (fun _ -> assert_equal false (dorm1_new.power_access));
]


let st6 = do' (Build (13,13,Road)) st5
let st7 = do' (Build (14,13,Road)) st6
let st8 = do' (Build (15,13,Road)) st7

let rroad1 = st2.grid.(13).(13)
let rroad2 = st3.grid.(14).(13)
let rroad3 = st4.grid.(15).(13)

let rroad_tests = [
  (*test right road connection*)
  "rroad1_btype" >:: (fun _ -> assert_equal Road (rroad1.btype));
  "rroad1_level" >:: (fun _ -> assert_equal 1 (rroad1.level));
  "rroad1_population" >:: (fun _ -> assert_equal 0 (rroad1.population));
  "rroad1_mcost">::
  (fun _ -> assert_equal (get_mcost Road) (rroad1.maintenance_cost));
  "rroad1_terrain" >:: (fun _ -> assert_equal Clear (rroad1.terrain));
  "rroad1_daccess" >:: (fun _ -> assert_equal false (rroad1.dining_access));
  "rroad1_laccess" >:: (fun _ -> assert_equal false (rroad1.lec_access));
  "rroad1_paccess" >:: (fun _ -> assert_equal false (rroad1.power_access));

  "st8_money">::
  (fun _ -> assert_equal
      (100000 -
       (get_bcost Dorm)-
       6*(get_bcost Road)-
       (get_bcost Lecture)) (st8.money));
]

let st9 = do' (Build (17,14,Dining)) st8
let din1 = st9.grid.(17).(14)
let dorm1_new2 = st9.grid.(11).(12)

let dining_tests = [
  (*check that dining hall was build correctly*)
  "din1_btype" >:: (fun _ -> assert_equal Dining (din1.btype));
  "din1_level" >:: (fun _ -> assert_equal 1 (din1.level));
  "din1_population" >:: (fun _ -> assert_equal 0 (din1.population));
  "din1_mcost">::
  (fun _ -> assert_equal (get_mcost Dining) (din1.maintenance_cost));
  "din1_terrain" >:: (fun _ -> assert_equal Clear (din1.terrain));
  "din1_daccess" >:: (fun _ -> assert_equal true (din1.dining_access));
  "din1_laccess" >:: (fun _ -> assert_equal false (din1.lec_access));
  "din1_paccess" >:: (fun _ -> assert_equal false (din1.power_access));

  (*check that state attributes are changed correctly*)
  "st9_happiness" >:: (fun _ -> assert_equal 50 (st9.happiness));
  "st9_money">::
  (fun _ -> assert_equal
      (100000 -
       (get_bcost Dorm)-
       6*(get_bcost Road)-
       (get_bcost Lecture)-
       (get_bcost Dining)) (st9.money));


  (*check that only dining access is changed*)
  "d_new1_daccess" >:: (fun _ -> assert_equal true (dorm1_new2.dining_access));
  "d_new1_laccess" >:: (fun _ -> assert_equal true (dorm1_new2.lec_access));
  "d_new1_paccess" >:: (fun _ -> assert_equal false (dorm1_new2.power_access));
]

let st10 = do' (Build (11,14, Pline)) st9
let st11 = do' (Build (11,16, Power)) st10
let dorm1_new3 = st11.grid.(11).(12)

let power_tests = [
(*check that power access is changed*)
"d_new2_daccess" >:: (fun _ -> assert_equal true (dorm1_new3.dining_access));
"d_new2_laccess" >:: (fun _ -> assert_equal true (dorm1_new3.lec_access));
"d_new2_paccess" >:: (fun _ -> assert_equal true (dorm1_new3.power_access));
]
      (*
let st12 = do' TimeStep st11

let timeStep_tests = [
  "st12_happiness" >:: (fun _ -> assert_equal 50 (st9.happiness));


]
  *)



let tests =
  "test suite for game"  >::: List.flatten [
    init_tests;
    dorm1_tests;
    lroad_tests;
    lecture_tests;
    rroad_tests;
    dining_tests;
    power_tests
  ]
let _ = run_test_tt_main tests
