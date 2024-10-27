open OUnit2
open Cs3110_final_project.Waypoint

(* helps *)
let wp1 = create_wp "Home" (0, 0)
let wp2 = create_wp "School" (10, 20)
let wp3 = create_wp "Park" (-5, 15)

(*module tests*)
let waypoint_tests =
  [
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
  ]

(* Main test suite *)
let suite =
  "CS3110 Final Project Test Suite"
  >::: [ "Waypoint Module Tests" >::: waypoint_tests ]

let () = run_test_tt_main suite
