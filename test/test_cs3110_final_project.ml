open OUnit2
open Cs3110_final_project.Waypoint

(* helps *)
let wp1 = create_wp "Home" (0, 0)
let wp2 = create_wp "School" (10, 20)
let wp3 = create_wp "Park" (-5, 15)

let make_create_wp_test expected name coords =
  "unit test for [create_wp]" >:: fun _ ->
  assert_equal expected (create_wp name coords)

let make_empty_test =
  "unit test for [empty]" >:: fun _ -> assert_equal [||] empty

let make_is_empty_test expected p =
  "unit test for [is_empty]" >:: fun _ -> assert_equal expected (is_empty p)

let make_contains_test expected wp p =
  "unit test for [contains]" >:: fun _ -> assert_equal expected (contains wp p)

let make_get_test expected i p =
  "unit test for [get]" >:: fun _ -> assert_equal expected (get i p)

let make_get_index_test expected wp p =
  "unit test for [get_index]" >:: fun _ ->
  assert_equal expected (get_index wp p)

let make_append_test expected wp p =
  "unit test for [append]" >:: fun _ -> assert_equal expected (append wp p)

let make_insert_test expected wp i p =
  "unit test for [insert]" >:: fun _ -> assert_equal expected (insert wp i p)

let make_remove_test expected i p =
  "unit test for [remove]" >:: fun _ -> assert_equal expected (remove i p)

let make_path_to_list_test expected p =
  "unit test for [path_to_list]" >:: fun _ ->
  assert_equal expected (path_to_list p)

(*module tests*)
let waypoint_tests =
  [
    make_create_wp_test { name = "Test"; coords = (1, 2) } "Test" (1, 2);
    make_empty_test;
    make_is_empty_test true empty;
    make_is_empty_test false [| wp1 |];
    make_contains_test true wp1 [| wp1 |];
    make_contains_test true wp1 [| wp1; wp2; wp3 |];
    make_contains_test true wp2 [| wp1; wp2; wp3 |];
    make_contains_test true wp3 [| wp1; wp2; wp3 |];
    make_get_test wp1 0 [| wp1 |];
    make_get_test wp1 0 [| wp1; wp2; wp3 |];
    make_get_test wp2 1 [| wp1; wp2; wp3 |];
    make_get_test wp3 2 [| wp1; wp2; wp3 |];
    make_get_index_test 0 wp1 [| wp1 |];
    make_get_index_test 0 wp1 [| wp1; wp2; wp3 |];
    make_get_index_test 1 wp2 [| wp1; wp2; wp3 |];
    make_get_index_test 2 wp3 [| wp1; wp2; wp3 |];
    make_append_test [| wp1 |] wp1 empty;
    make_append_test [| wp1; wp2 |] wp2 [| wp1 |];
    make_append_test [| wp1; wp2; wp3 |] wp3 [| wp1; wp2 |];
    make_insert_test [| wp1 |] wp1 0 [||];
    make_insert_test [| wp1; wp2; wp3 |] wp1 0 [| wp2; wp3 |];
    make_insert_test [| wp1; wp2; wp3 |] wp2 1 [| wp1; wp3 |];
    make_insert_test [| wp1; wp2; wp3 |] wp3 2 [| wp1; wp2 |];
    make_remove_test [||] 0 [| wp1 |];
    make_remove_test [| wp2; wp3 |] 0 [| wp1; wp2; wp3 |];
    make_remove_test [| wp1; wp3 |] 1 [| wp1; wp2; wp3 |];
    make_remove_test [| wp1; wp2 |] 2 [| wp1; wp2; wp3 |];
    ( "test_waypoint_creation" >:: fun _ ->
      let wp = create_wp "Test" (1, 2) in
      assert_equal "Test" wp.name ~msg:"Waypoint name should match";
      assert_equal (1, 2) wp.coords ~msg:"Waypoint coordinates should match" );
    ( "test_empty" >:: fun _ ->
      assert_equal 0 (Array.length empty) ~msg:"Empty path should have length 0"
    );
    ( "test_is_empty" >:: fun _ ->
      assert_equal true (is_empty empty) ~msg:"Empty path should be empty";
      assert_equal false (is_empty [| wp1 |])
        ~msg:"Non-empty path should not be empty" );
    ( "test_append" >:: fun _ ->
      let p1 = empty in
      let p2 = append wp1 p1 in
      let p3 = append wp2 p2 in
      assert_equal 1 (Array.length p2)
        ~msg:"Path length should be 1 after first append";
      assert_equal 2 (Array.length p3)
        ~msg:"Path length should be 2 after second append";
      assert_equal wp1 p3.(0) ~msg:"First waypoint should match";
      assert_equal wp2 p3.(1) ~msg:"Second waypoint should match" );
    ( "test_remove" >:: fun _ ->
      let p1 = append wp1 empty in
      let p2 = append wp2 p1 in
      let p3 = append wp3 p2 in
      let p4 = remove 1 p3 in
      assert_equal 2 (Array.length p4)
        ~msg:"Path length should be 2 after removal";
      assert_equal wp1 p4.(0) ~msg:"First waypoint should match";
      assert_equal wp3 p4.(1) ~msg:"Second waypoint should match" );
    ( "test_remove_empty" >:: fun _ ->
      assert_raises Empty (fun () -> remove 0 empty) );
    make_path_to_list_test [] empty;
    make_path_to_list_test [ wp1 ] [| wp1 |];
    make_path_to_list_test [ wp1; wp2; wp3 ] [| wp1; wp2; wp3 |];
  ]

(* Main test suite *)
let suite =
  "CS3110 Final Project Test Suite"
  >::: [ "Waypoint Module Tests" >::: waypoint_tests ]

let () = run_test_tt_main suite
