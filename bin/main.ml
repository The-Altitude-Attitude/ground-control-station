open Bogue
module W = Widget
module L = Layout
open Cs3110_final_project.Waypoint

let map_layout_ref = ref None
let map_scroll_ref = ref None
let map_file = ref "data/cornell.png"
let map = ref (L.resident (W.image (*~w:1000 ~h:600*) !map_file))

let plane_icon (x, y) =
  L.resident ~x ~y ~w:50 ~h:50 ~draggable:true
    (W.image_from_svg "data/Plane_icon.svg")

let wp_icon (x, y) =
  let dot_size = 10 in
  let centered_x = x - (dot_size / 2) in
  let centered_y = y - (dot_size / 2) in
  L.resident ~x:centered_x ~y:centered_y ~w:dot_size ~h:dot_size
    (W.image "data/waypoint_dot.png")

let waypoints = ref []

(* update map with new waypoints *)
let wp_update_map () =
  let waypoint_layouts = List.map (fun wp -> wp_icon wp.coords) !waypoints in
  let combined_layout =
    L.superpose (!map :: plane_icon (150, 300) :: waypoint_layouts)
  in
  match !map_layout_ref with
  | Some layout -> L.set_rooms layout [ combined_layout ]
  | None -> ()

let add_waypoint (x, y) =
  let wp =
    create_wp (Printf.sprintf "WP%d" (List.length !waypoints + 1)) (x, y)
  in
  waypoints := wp :: !waypoints;
  flush stdout;
  wp_update_map ()

(* update map image *)
let update_map file =
  waypoints := [];
  map_file := file;
  map := L.resident (W.image !map_file);
  W.on_click (L.widget !map) ~click:(fun _ ->
      let x, y = Mouse.pos () in
      let map_x = L.xpos !map in
      let map_y = L.ypos !map in
      let adjusted_x = x - map_x in
      let adjusted_y = y - map_y in
      add_waypoint (adjusted_x, adjusted_y));
  let waypoint_layouts = List.map (fun wp -> wp_icon wp.coords) !waypoints in
  let combined_layout =
    L.superpose (!map :: plane_icon (150, 300) :: waypoint_layouts)
  in
  match !map_layout_ref with
  | Some layout -> L.set_rooms layout [ combined_layout ]
  | None -> ()

let map_menu =
  let map1 =
    {
      Menu.label = Text "Cornell University";
      content = Action (fun () -> update_map "data/cornell.png");
    }
  in
  let map2 =
    {
      Menu.label = Text "Ovid Airport";
      content = Action (fun () -> update_map "data/ovid.png");
    }
  in
  let map3 =
    {
      Menu.label = Text "Mojave Desert";
      content = Action (fun () -> update_map "data/mojave.png");
    }
  in
  let map_options = [ map1; map2; map3 ] in
  { Menu.label = Text "Change Map"; content = Tower map_options }

let () =
  update_map !map_file;

  let menu = L.flat_of_w [ W.text_display "Menu" ] in
  let map_comb = L.superpose [ !map; plane_icon (150, 300) ] in
  let map_layout = L.flat ~margins:0 [ map_comb ] in
  map_layout_ref := Some map_layout;
  let map_scroll = L.make_clip ~w:900 ~h:600 map_layout in
  map_scroll_ref := Some map_scroll;
  let layout = L.flat [ menu; map_scroll ] in
  Menu.add_bar ~dst:menu [ map_menu ];
  let gcs = Bogue.of_layout layout in
  Bogue.run gcs
