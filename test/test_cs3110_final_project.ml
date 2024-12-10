open OUnit2
open Cs3110_final_project.Waypoint

(* helps *)
let wp1 = create_wp "Home" (0, 0)
let wp2 = create_wp "School" (10, 20)
let wp3 = create_wp "Park" (-5, 15)
let wp1_todo = { wp1 with status = ToDo }
let wp2_pending = { wp2 with status = Pending }
let wp3_done = { wp3 with status = Done }
let string_printer s = s
let coords_printer (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let status_printer = function
  | ToDo -> "ToDo"
  | Pending -> "Pending"
  | Done -> "Done"

let wp_printer wp =
  "{ " ^ name wp ^ "; "
  ^ coords_printer (coords wp)
  ^ "; "
  ^ status_printer (status wp)
  ^ " }"

let path_printer p =
  let path = path_to_list p in
  let rec str p s =
    match p with
    | [] -> "[| |]"
    | [ h ] -> s ^ wp_printer h ^ " |]"
    | h :: t -> str t (s ^ wp_printer h ^ "; ")
  in
  "[|" ^ str path ""

let list_printer lst =
  let rec str lst s =
    match lst with
    | [] -> "]"
    | [ h ] -> s ^ wp_printer h ^ "]"
    | h :: t -> str t (s ^ wp_printer h ^ "; ")
  in
  "[" ^ str lst ""

let make_create_wp_test expected name coords =
  "unit test for [create_wp]" >:: fun _ ->
  assert_equal expected (create_wp name coords) ~printer:wp_printer

let make_name_test expected wp =
  "unit test for [name]" >:: fun _ ->
  assert_equal expected (name wp) ~printer:string_printer

let make_coords_test expected wp =
  "unit test for [coords]" >:: fun _ ->
  assert_equal expected (coords wp) ~printer:coords_printer

let make_status_test expected wp =
  "unit test for [status]" >:: fun _ ->
  assert_equal expected (status wp) ~printer:status_printer

let make_set_status_test initial_status new_status expected wp =
  "unit test for [set_status]" >:: fun _ ->
  set_status wp initial_status;
  set_status wp new_status;
  assert_equal expected wp.status ~printer:status_printer

let make_empty_test =
  "unit test for [empty]" >:: fun _ ->
  assert_equal [||] empty ~printer:path_printer

let make_is_empty_test expected p =
  "unit test for [is_empty]" >:: fun _ ->
  assert_equal expected (is_empty p) ~printer:string_of_bool

let make_length_test expected p =
  "unit test for [length]" >:: fun _ ->
  assert_equal expected (length p) ~printer:string_of_int

let make_contains_test expected wp p =
  "unit test for [contains]" >:: fun _ ->
  assert_equal expected (contains wp p) ~printer:string_of_bool

let make_get_test expected i p =
  "unit test for [get]" >:: fun _ ->
  assert_equal expected (get i p) ~printer:wp_printer

let make_get_index_test expected wp p =
  "unit test for [get_index]" >:: fun _ ->
  assert_equal expected (get_index wp p) ~printer:string_of_int

let make_append_test expected wp p =
  "unit test for [append]" >:: fun _ ->
  assert_equal expected (append wp p) ~printer:path_printer

let make_insert_test expected wp i p =
  "unit test for [insert]" >:: fun _ ->
  assert_equal expected (insert wp i p) ~printer:path_printer

let make_remove_test expected i p =
  "unit test for [remove]" >:: fun _ ->
  assert_equal expected (remove i p) ~printer:path_printer

let make_path_to_list_test expected p =
  "unit test for [path_to_list]" >:: fun _ ->
  assert_equal expected (path_to_list p) ~printer:list_printer

let make_distance_test expected wp1 wp2 =
  "unit test for [distance]" >:: fun _ ->
  assert_equal expected (distance wp1 wp2) ~printer:string_of_int

(*module tests*)
let waypoint_tests =
  [
    make_create_wp_test
      { name = "Test"; coords = (1, 2); status = ToDo }
      "Test" (1, 2);
    make_name_test "Home" wp1;
    make_name_test "School" wp2;
    make_name_test "Park" wp3;
    make_coords_test (0, 0) wp1;
    make_coords_test (10, 20) wp2;
    make_coords_test (-5, 15) wp3;
    make_status_test ToDo wp1_todo;
    make_status_test Pending wp2_pending;
    make_status_test Done wp3_done;
    make_set_status_test ToDo ToDo ToDo wp1_todo;
    make_set_status_test ToDo Done Done wp2_pending;
    make_set_status_test Done ToDo ToDo wp3_done;
    make_empty_test;
    make_is_empty_test true empty;
    make_is_empty_test false [| wp1 |];
    make_length_test 0 [||];
    make_length_test 1 [| wp1 |];
    make_length_test 3 [| wp1; wp2; wp3 |];
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
    make_distance_test 22 (coords wp1) (coords wp2);
    make_distance_test 0 (coords wp2) (coords wp2);
    make_distance_test 15 (coords wp3) (coords wp2);
    make_distance_test 0 (coords wp1) (coords wp1);
  ]

(* Main test suite *)
let suite =
  "CS3110 Final Project Test Suite"
  >::: [ "Waypoint Module Tests" >::: waypoint_tests ]

let () = run_test_tt_main suite
