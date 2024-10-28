open Bogue
open Waypoint
module W = Widget
module L = Layout

let wp_list = ref []
let wp_widget _wp = W.image "waypoint_dot.png" ~noscale:true

let wp_layout wp =
  let x, y = wp.coords in
  let dot_size = 10 in
  (* Make dot smaller *)
  let centered_x = x - (dot_size / 2) in
  let centered_y = y - (dot_size / 2) in
  L.resident ~x:centered_x ~y:centered_y ~w:dot_size ~h:dot_size (wp_widget wp)

let wp_map_render map_layout waypoints =
  let waypoint_layouts = List.map wp_layout waypoints in
  L.superpose (map_layout :: waypoint_layouts)

let map_ref = ref None

let add_waypoint x y =
  let wp =
    create_wp (Printf.sprintf "WP%d" (List.length !wp_list + 1)) (x, y)
  in
  wp_list := wp :: !wp_list;
  match !map_ref with
  | Some map_layout ->
      let new_layout = wp_map_render map_layout !wp_list in
      L.set_rooms map_layout [ new_layout ]
  | None -> ()

(* Map layout with click handling *)
let wp_layout_map map_image_path =
  let map = W.image ~noscale:true map_image_path in
  let map_layout = L.resident map in
  map_ref := Some map_layout;

  W.on_click map ~click:(fun _w ->
      let x, y = Mouse.pos () in
      let adjusted_x = x - L.xpos map_layout in
      let adjusted_y = y - L.ypos map_layout in
      add_waypoint adjusted_x adjusted_y);

  wp_map_render map_layout !wp_list
