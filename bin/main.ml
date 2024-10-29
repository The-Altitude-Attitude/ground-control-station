open Bogue
module W = Widget
module L = Layout
open Cs3110_final_project.Waypoint

let plane_icon (x, y) =
  L.resident ~x ~y ~w:50 ~h:50 ~draggable:true
    (W.image_from_svg "Plane_icon.svg")

let wp_icon (x, y) =
  let dot_size = 10 in
  let centered_x = x - (dot_size / 2) in
  let centered_y = y - (dot_size / 2) in
  L.resident ~x:centered_x ~y:centered_y ~w:dot_size ~h:dot_size
    (W.image "waypoint_dot.png" ~noscale:true)

let gcs_map =
  let map = W.image "map.png" in
  L.resident map

let waypoints = ref []
let map_scroll_ref = ref None

(* update map with new waypoints *)
let wp_update_map () =
  let waypoint_layouts = List.map (fun wp -> wp_icon wp.coords) !waypoints in
  let updated_layout =
    L.superpose (gcs_map :: plane_icon (150, 300) :: waypoint_layouts)
  in
  let updated_scroll = L.make_clip ~w:900 ~h:600 updated_layout in
  match !map_scroll_ref with
  | Some scroll -> L.set_rooms scroll [ updated_scroll ]
  | None -> ()

let add_waypoint (x, y) =
  let wp =
    create_wp (Printf.sprintf "WP%d" (List.length !waypoints + 1)) (x, y)
  in
  waypoints := wp :: !waypoints;
  flush stdout;
  wp_update_map ()

let map_menu =
  let map1 =
    {
      Menu.label = Text "Map 1";
      content = Action (fun () -> print_endline "Map 1");
    }
  in
  let map2 =
    {
      Menu.label = Text "Map 2";
      content = Action (fun () -> print_endline "Map 2");
    }
  in
  let map3 =
    {
      Menu.label = Text "Map 3";
      content = Action (fun () -> print_endline "Map 2");
    }
  in
  let map_options = [ map1; map2; map3 ] in
  { Menu.label = Text "Map Options"; content = Tower map_options }

let () =
  W.on_click (L.widget gcs_map) ~click:(fun _ ->
      let x, y = Mouse.pos () in
      let map_x = L.xpos gcs_map in
      let map_y = L.ypos gcs_map in
      let adjusted_x = x - map_x in
      let adjusted_y = y - map_y in
      (* Printf.printf
            "click: (%d, %d), map ajusted pos: (%d, %d)\n"
         x y adjusted_x adjusted_y;
         flush stdout; *)
      add_waypoint (adjusted_x, adjusted_y));
  let menu = L.flat_of_w [ W.text_display "Menu" ] in
  let map_comb = L.superpose [ gcs_map; plane_icon (150, 300) ] in
  let map_scroll = L.make_clip ~w:900 ~h:600 map_comb in
  map_scroll_ref := Some map_scroll;
  let layout = L.flat [ menu; map_scroll ] in
  let () = Menu.add_bar ~dst:menu [ map_menu ] in
  let gcs = Bogue.of_layout layout in
  Bogue.run gcs
