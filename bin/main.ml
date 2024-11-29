open Bogue
module W = Widget
module L = Layout
open Cs3110_final_project.Waypoint

let map_layout_ref = ref None
let map_scroll_ref = ref None
let wp_table_ref = ref None
let map_file = ref "data/cornell.png" (* default to cornell *)
let map = ref (L.resident (W.image !map_file))
let waypoints = ref empty
let wp_table = ref (L.resident (W.label "No Waypoints"))

(* map options *)
let map_options = [| "Cornell University"; "Ovid Airport"; "Mojave Desert" |]
let map_files = [| "data/cornell.png"; "data/ovid.png"; "data/mojave.png" |]

(* icons *)
let create_icon file ~x ~y ~w ~h =
  L.resident ~x ~y ~w ~h ~draggable:false (W.image file)

let plane_icon (x, y) = create_icon "data/Plane_icon.svg" ~x ~y ~w:50 ~h:50

let wp_icon (x, y) =
  let size = 10 in
  let centered_x, centered_y = (x - (size / 2), y - (size / 2)) in
  create_icon "data/waypoint_dot.png" ~x:centered_x ~y:centered_y ~w:size
    ~h:size

let reset_waypoints () = waypoints := empty

let add_waypoint (x, y) =
  let wp = create_wp (Printf.sprintf "WP%d" (length !waypoints + 1)) (x, y) in
  waypoints := append wp !waypoints;
  flush stdout

let update_map_layout () =
  let waypoint_icons =
    Array.to_list (Array.map (fun wp -> wp_icon wp.coords) !waypoints)
  in
  let combined_layout =
    L.superpose (!map :: plane_icon (150, 300) :: waypoint_icons)
  in
  match !map_layout_ref with
  | Some layout -> L.set_rooms layout [ combined_layout ]
  | None -> ()

let update_wp_table () =
  (wp_table :=
     let length = length !waypoints in
     if length = 0 then L.resident (W.label "No Waypoints")
     else
       let create_column title width getter =
         Table.
           {
             title;
             length;
             rows =
               (fun i ->
                 match get i !waypoints with
                 | wp -> L.resident (W.label (getter wp))
                 | exception _ -> L.resident (W.label "Invalid Waypoint"));
             compare = Some (fun i j -> compare i j);
             width = Some width;
           }
       in
       let col_name = create_column "Waypoint" 80 (fun wp -> wp.name) in
       let col_x =
         create_column "x" 30 (fun wp -> string_of_int (fst wp.coords))
       in
       let col_y =
         create_column "y" 30 (fun wp -> string_of_int (snd wp.coords))
       in
       fst (Table.create ~h:500 [ col_name; col_x; col_y ]));

  match !wp_table_ref with
  | Some layout -> L.set_rooms layout [ L.superpose [ !wp_table ] ]
  | None -> ()

(* handle map updates *)
let update_map file =
  reset_waypoints ();
  map_file := file;
  map := L.resident (W.image !map_file);

  W.on_click (L.widget !map) ~click:(fun _ ->
      let x, y = Mouse.pos () in
      let map_x, map_y = (L.xpos !map, L.ypos !map) in
      add_waypoint (x - map_x, y - map_y);
      update_wp_table ();
      update_map_layout ());

  update_wp_table ();
  update_map_layout ()

(* ui *)
let map_menu =
  let map_select =
    Select.create
      ~action:(fun index -> update_map map_files.(index))
      map_options 0
  in
  let label = W.label "Select Map:" in
  L.flat ~margins:10 [ L.resident label; map_select ]

let clear_path_button =
  let button = W.button "Clear Path" in
  W.on_click button ~click:(fun _ ->
      reset_waypoints ();
      update_wp_table ();
      update_map_layout ());
  L.resident button

(** app setup *)
let init_app () =
  update_map !map_file;

  let map_combined = L.superpose [ !map; plane_icon (150, 300) ] in
  let map_layout = L.flat ~margins:0 [ map_combined ] in
  map_layout_ref := Some map_layout;

  let map_scroll = L.make_clip ~w:900 ~h:600 map_layout in
  map_scroll_ref := Some map_scroll;

  let wp_table_super = L.superpose [ !wp_table ] in
  wp_table_ref := Some wp_table_super;

  let sidebar = L.tower [ map_menu; clear_path_button; wp_table_super ] in
  let main_layout = L.flat ~sep:20 [ sidebar; map_scroll ] in
  Bogue.of_layout main_layout

(** entry point *)
let () =
  let gcs = init_app () in
  Bogue.run gcs
