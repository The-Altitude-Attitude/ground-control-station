(* let () = print_endline "Hello, World!" *)
open Bogue
module W = Widget
module L = Layout

let gcs_map =
  let map = W.image ~noscale:true "map.png" in
  L.resident map

let () =
  let menu = L.flat_of_w [ W.text_display "Menu" ] in
  let map_scroll = L.make_clip ~w:900 ~h:600 gcs_map in
  let layout = L.flat [ menu; map_scroll ] in
  let gcs = Bogue.of_layout layout in
  Bogue.run gcs
