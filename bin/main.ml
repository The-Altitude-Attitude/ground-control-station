open Bogue
module W = Widget
module L = Layout

let plane_icon (x, y) =
  L.resident ~x ~y ~w:50 ~h:50 ~draggable:true
    (W.image_from_svg "Plane_icon.svg")

let gcs_map =
  let map = W.image ~noscale:true "map.png" in
  L.resident map

let () =
  let menu = L.flat_of_w [ W.text_display "Menu" ] in
  let map_comb = L.superpose [ gcs_map; plane_icon (150, 300) ] in
  let map_scroll = L.make_clip ~w:900 ~h:600 map_comb in
  let layout = L.flat [ menu; map_scroll ] in
  let gcs = Bogue.of_layout layout in
  Bogue.run gcs
