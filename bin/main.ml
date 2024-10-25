(* let () = print_endline "Hello, World!" *)
open Bogue
module W = Widget
module L = Layout

let dark_blue_line =
  Style.mk_line
    ~color:Draw.(opaque @@ find_color "deepskyblue")
    ~width:2 ~style:Solid ()

let gcs_map =
  let open Style in
  let border = mk_border ~radius:10 dark_blue_line in
  let p = Image.create ~bg:Draw.(opaque white) "map.png" in
  create ~border ~background:(image_bg p) ()

let () =
  let menu = W.text_display "Menu" in
  let map = W.box ~style:gcs_map () in
  let layout = L.flat_of_w [ menu; map ] in
  let gcs = Bogue.of_layout layout in
  Bogue.run gcs
