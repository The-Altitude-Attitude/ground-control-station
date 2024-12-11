open OUnit2
open Cs3110_final_project.Waypoint

(* waypoints to test *)
let wp1 = create_wp "A" (0, 0)
let wp2 = create_wp "B" (3, 4)
let wp3 = create_wp "C" (-5, -5)
let wp4 = create_wp "D" (10, 10)
let wp5 = create_wp "E" (-3, -4)
let wp6 = create_wp "F" (0, 5)

(* printers *)
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

(* tests *)
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

let make_set_status_test s wp =
  "unit test for [set_status]" >:: fun _ ->
  set_status wp s;
  assert_equal s (status wp) ~printer:status_printer

let make_empty_test expected =
  "unit test for [empty]" >:: fun _ ->
  assert_equal expected empty ~printer:path_printer

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

let create_wp_tests =
  [
    make_create_wp_test { name = "A"; coords = (0, 0); status = ToDo } "A" (0, 0);
    make_create_wp_test { name = "B"; coords = (3, 4); status = ToDo } "B" (3, 4);
    make_create_wp_test
      { name = "C"; coords = (-5, -5); status = ToDo }
      "C" (-5, -5);
    make_create_wp_test
      { name = "D"; coords = (10, 10); status = ToDo }
      "D" (10, 10);
    make_create_wp_test
      { name = "E"; coords = (-3, -4); status = ToDo }
      "E" (-3, -4);
    make_create_wp_test { name = "F"; coords = (0, 5); status = ToDo } "F" (0, 5);
  ]

let name_tests =
  [
    make_name_test "A" wp1;
    make_name_test "B" wp2;
    make_name_test "C" wp3;
    make_name_test "D" wp4;
    make_name_test "E" wp5;
    make_name_test "F" wp6;
  ]

let coords_tests =
  [
    make_coords_test (0, 0) wp1;
    make_coords_test (3, 4) wp2;
    make_coords_test (-5, -5) wp3;
    make_coords_test (10, 10) wp4;
    make_coords_test (-3, -4) wp5;
    make_coords_test (0, 5) wp6;
  ]

let status_tests =
  [
    make_status_test ToDo wp1;
    make_status_test ToDo wp2;
    make_status_test ToDo wp3;
  ]

let set_status_tests =
  [
    make_set_status_test Pending wp1;
    make_set_status_test Pending wp2;
    make_set_status_test Pending wp3;
    make_set_status_test Done wp1;
    make_set_status_test Done wp2;
    make_set_status_test Done wp3;
    make_set_status_test ToDo wp1;
    make_set_status_test ToDo wp2;
    make_set_status_test ToDo wp3;
  ]

let empty_test = [ make_empty_test [||] ]

let is_empty_tests =
  [
    make_is_empty_test true empty;
    make_is_empty_test false (append wp1 empty);
    make_is_empty_test false (append wp2 (append wp1 empty));
    make_is_empty_test true (remove 0 (append wp1 empty));
  ]

let length_tests =
  [
    make_length_test 0 empty;
    make_length_test 1 (append wp1 empty);
    make_length_test 2 (append wp2 (append wp1 empty));
    make_length_test 3 (append wp3 (append wp2 (append wp1 empty)));
    make_length_test 0 (remove 0 (append wp1 empty));
  ]

let contains_tests =
  [
    make_contains_test false wp1 empty;
    make_contains_test true wp1 (append wp1 empty);
    make_contains_test false wp2 (append wp1 empty);
    make_contains_test true wp1 (append wp2 (append wp1 empty));
    make_contains_test true wp2 (append wp2 (append wp1 empty));
    make_contains_test true wp1 [| wp1; wp2; wp3 |];
    make_contains_test true wp2 [| wp1; wp2; wp3 |];
    make_contains_test true wp3 [| wp1; wp2; wp3 |];
    make_contains_test false wp4 [| wp1; wp2; wp3 |];
    make_contains_test false wp5 [| wp1; wp2; wp3 |];
    make_contains_test false wp6 [| wp1; wp2; wp3 |];
  ]

let get_tests =
  [
    make_get_test wp1 0 [| wp1 |];
    make_get_test wp1 0 [| wp1; wp2; wp3 |];
    make_get_test wp2 1 [| wp1; wp2; wp3 |];
    make_get_test wp3 2 [| wp1; wp2; wp3 |];
    make_get_test wp1 0 [| wp1; wp2; wp3; wp2; wp1 |];
    make_get_test wp1 4 [| wp1; wp2; wp3; wp2; wp1 |];
    make_get_test wp2 1 [| wp1; wp2; wp2; wp3 |];
    make_get_test wp3 3 [| wp1; wp2; wp2; wp3 |];
  ]

let get_index_tests =
  [
    make_get_index_test 0 wp1 [| wp1 |];
    make_get_index_test 0 wp1 [| wp1; wp2; wp3 |];
    make_get_index_test 1 wp2 [| wp1; wp2; wp3 |];
    make_get_index_test 2 wp3 [| wp1; wp2; wp3 |];
    make_get_index_test 0 wp1 [| wp1; wp2; wp3; wp1 |];
    make_get_index_test 1 wp2 [| wp1; wp2; wp3; wp2 |];
    make_get_index_test 0 wp1 [| wp1; wp2; wp3; wp1 |];
    make_get_index_test 2 wp3 [| wp1; wp2; wp3; wp2; wp3 |];
  ]

let append_tests =
  [
    make_append_test [| wp1 |] wp1 [||];
    make_append_test [| wp1; wp2 |] wp2 [| wp1 |];
    make_append_test [| wp1; wp2; wp3 |] wp3 [| wp1; wp2 |];
    make_append_test [| wp1; wp2; wp3 |] wp3 [| wp1; wp2 |];
    make_append_test [| wp1; wp2; wp3; wp4 |] wp4 [| wp1; wp2; wp3 |];
    make_append_test [| wp1; wp1 |] wp1 [| wp1 |];
    make_append_test [| wp1; wp1; wp1 |] wp1 [| wp1; wp1 |];
    make_append_test [| wp1; wp2; wp3; wp4 |] wp4 [| wp1; wp2; wp3 |];
    make_append_test [| wp1; wp2; wp3; wp4; wp5 |] wp5 [| wp1; wp2; wp3; wp4 |];
    make_append_test [| wp1 |] wp1 empty;
    make_append_test [| wp1; wp2 |] wp2 [| wp1 |];
    make_append_test [| wp1; wp3 |] wp3 [| wp1 |];
    make_append_test [| wp1; wp2 |] wp2 [| wp1 |];
    make_append_test [| wp1; wp2; wp3 |] wp3 [| wp1; wp2 |];
  ]

let insert_tests =
  [
    make_insert_test [| wp1 |] wp1 0 empty;
    make_insert_test [| wp1; wp2 |] wp1 0 [| wp2 |];
    make_insert_test [| wp1; wp2 |] wp1 0 [| wp2 |];
    make_insert_test [| wp1; wp2 |] wp2 1 [| wp1 |];
    make_insert_test [| wp1; wp2; wp3 |] wp3 2 [| wp1; wp2 |];
    make_insert_test [| wp1; wp2; wp3 |] wp2 1 [| wp1; wp3 |];
    make_insert_test [| wp1; wp2; wp3 |] wp3 2 [| wp1; wp2 |];
    make_insert_test [| wp1; wp2; wp3; wp4 |] wp4 3 [| wp1; wp2; wp3 |];
  ]

let remove_tests =
  [
    make_remove_test [| wp2; wp3 |] 0 [| wp1; wp2; wp3 |];
    make_remove_test [| wp1; wp3 |] 1 [| wp1; wp2; wp3 |];
    make_remove_test [| wp1; wp2 |] 2 [| wp1; wp2; wp3 |];
    make_remove_test [||] 0 [| wp1 |];
  ]

let path_to_list_tests =
  [
    make_path_to_list_test [] empty;
    make_path_to_list_test [ wp1 ] [| wp1 |];
    make_path_to_list_test [ wp1; wp2; wp3 ] [| wp1; wp2; wp3 |];
  ]

let distance_tests =
  [
    make_distance_test 0 (coords wp1) (coords wp1);
    make_distance_test 5 (coords wp1) (coords wp2);
    make_distance_test 21 (coords wp3) (coords wp4);
    make_distance_test 9 (coords wp5) (coords wp6);
    make_distance_test 9 (coords wp2) (coords wp4);
  ]

let waypoint_tests =
  [
    ( "test_remove_empty" >:: fun _ ->
      assert_raises Empty (fun () -> remove 0 empty) );
  ]

(* Main test suite *)
let suite =
  "CS3110 Final Project Test Suite"
  >::: [
         "Create Waypoint Tests" >::: create_wp_tests;
         "Name Tests" >::: name_tests;
         "Coords Tests" >::: coords_tests;
         "Status Tests" >::: status_tests;
         "Set Status Tests" >::: set_status_tests;
         "Empty Test" >::: empty_test;
         "Is Empty Tests" >::: is_empty_tests;
         "Length Tests" >::: length_tests;
         "Contains Tests" >::: contains_tests;
         "Get Tests" >::: get_tests;
         "Get Index Tests" >::: get_index_tests;
         "Append Tests" >::: append_tests;
         "Insert Tests" >::: insert_tests;
         "Remove Tests" >::: remove_tests;
         "Path to List Tests" >::: path_to_list_tests;
         "Distance Tests" >::: distance_tests;
         "Waypoint Tests" >::: waypoint_tests;
       ]

let () = run_test_tt_main suite
