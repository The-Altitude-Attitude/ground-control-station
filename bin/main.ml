(* let () = print_endline "Hello, World!" *)
open Bogue
module W = Widget
module L = Layout

let gcs_map =
  let p = Image.create ~noscale:false ~bg:Draw.(opaque white) "map.png" in
  let map_background = Style.create ~background:(Style.image_bg p) () in
  let map_box = W.box ~w:600 ~h:600 ~style:map_background () in
  L.resident ~w:900 ~h:600 ~background:(L.style_bg map_background) map_box

let () =
  let menu = L.flat_of_w [ W.text_display "Menu" ] in
  (* let container = L.make_clip ~h:300 long in *)
  let layout = L.flat [ menu; gcs_map ] in
  let gcs = Bogue.of_layout layout in
  Bogue.run gcs
