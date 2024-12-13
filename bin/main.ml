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
let speed = ref 10 (* default speed *)
let simulation_running = ref false (* default simulation not running*)
let message_label = ref (W.label "No messages")
let update_message new_message = W.set_text !message_label new_message

(* horizontal scroll offset reference *)
let offset_x = ref 0

(* map options *)
let map_options = [| "Cornell University"; "Ovid Airport"; "Mojave Desert" |]
let map_files = [| "data/cornell.png"; "data/ovid.png"; "data/mojave.png" |]

(* icons *)
let create_icon file ~x ~y ~w ~h draggable =
  L.resident ~x ~y ~w ~h ~draggable (W.image file)

let plane_icon (x, y) = create_icon "data/Plane_icon.svg" ~x ~y ~w:50 ~h:50 true
let plane_initialized = ref false (* if plane isn't in animation *)
let plane_ref = ref (plane_icon (150, 300))

let wp_icon (x, y) =
  let size = 10 in
  let centered_x, centered_y = (x - (size / 2), y - (size / 2)) in
  create_icon "data/waypoint_dot.png" ~x:centered_x ~y:centered_y ~w:size
    ~h:size true

(* map updates *)
let reset_waypoints () = waypoints := empty
let update_wp_status idx new_status = set_status (get idx !waypoints) new_status

let reset_waypoint_statuses () =
  Array.iter (fun wp -> set_status wp ToDo) !waypoints

let add_waypoint (x, y) =
  let wp = create_wp (Printf.sprintf "WP%d" (length !waypoints + 1)) (x, y) in
  waypoints := append wp !waypoints;
  update_message
    (Printf.sprintf "Added waypoint %d at (%d, %d)" (length !waypoints) x y);
  flush stdout

let update_offset new_offset =
  offset_x := new_offset;
  L.setx !map !offset_x

let update_map_layout () =
  let waypoint_icons =
    Array.to_list
      (Array.map
         (fun wp ->
           let x, y = coords wp in
           wp_icon (x + !offset_x, y))
         !waypoints)
  in

  let plane_icon_dynamic =
    if !plane_initialized then !plane_ref
    else
      let xpos = L.getx !plane_ref in
      let ypos = L.gety !plane_ref in
      plane_icon (xpos + !offset_x, ypos)
  in

  let combined_layout =
    L.superpose (!map :: plane_icon_dynamic :: waypoint_icons)
  in

  match !map_layout_ref with
  | Some layout -> L.set_rooms layout [ combined_layout ]
  | None -> ()

let apply_offset () =
  L.setx !map !offset_x;
  update_map_layout ()

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
                 | wp -> L.resident (getter wp)
                 | exception _ -> L.resident (W.label "Invalid Waypoint"));
             compare = Some (fun i j -> compare i j);
             width = Some width;
           }
       in
       let col_name =
         create_column "Waypoint" 80 (fun wp -> W.label (name wp))
       in
       let col_x =
         create_column "x" 30 (fun wp ->
             W.label (string_of_int (fst (coords wp))))
       in
       let col_y =
         create_column "y" 30 (fun wp ->
             W.label (string_of_int (snd (coords wp))))
       in
       let col_status =
         create_column "Status" 50 (fun wp ->
             let status_icon =
               match status wp with
               | ToDo -> W.icon ~size:20 ~fg:(220, 20, 60, 0) "times-circle"
               | Pending -> W.icon ~size:20 ~fg:(255, 215, 0, 0) "repeat"
               | Done -> W.icon ~size:20 ~fg:(34, 139, 34, 0) "check-circle"
             in
             status_icon)
       in
       fst (Table.create ~h:500 [ col_name; col_x; col_y; col_status ]));

  match !wp_table_ref with
  | Some layout -> L.set_rooms layout [ L.superpose [ !wp_table ] ]
  | None -> ()

(* handle map updates *)
let update_map file =
  reset_waypoints ();
  map_file := file;
  map := L.resident (W.image !map_file);

  (* if the map is a new one, keep the offset to 0 *)
  update_offset 0;

  W.on_click (L.widget !map) ~click:(fun _ ->
      let x, y = Mouse.pos () in
      let map_x, map_y = (L.xpos !map, L.ypos !map) in
      add_waypoint (x - map_x, y - map_y);
      update_wp_table ();
      apply_offset ());

  update_wp_table ();
  update_map_layout ()

(* ui *)

(* speed slider logic*)

let speed_slider =
  let slider_label = W.label (string_of_int !speed) in
  let slider =
    W.slider_with_action ~value:!speed ~length:150 ~step:1
      ~kind:Slider.Horizontal
      ~action:(fun value ->
        if not !simulation_running then (
          speed := 20 - (value - 1);
          W.set_text slider_label (string_of_int (22 - !speed)))
        else update_message "Can't change speed while running simulation!")
      19
  in
  L.tower
    [
      L.resident (W.label "Set Speed:");
      L.flat ~align:Draw.Center
        [
          L.resident ~background:(L.color_bg (211, 211, 211, 100)) slider;
          L.resident slider_label;
        ];
    ]

(* map selection logic *)

let map_menu =
  let map_select =
    Select.create
      ~action:(fun index ->
        if not !simulation_running then update_map map_files.(index)
        else update_message "Can't change map while running simulation!")
      map_options 0
  in
  let label = W.label "Select Map:" in
  L.flat ~margins:10 [ L.resident label; map_select ]

(* clear waypoint path logic *)

let clear_path_button =
  let button = W.button "Clear Path" in
  W.on_click button ~click:(fun _ ->
      if not !simulation_running then (
        reset_waypoints ();
        update_wp_table ();
        update_map_layout ())
      else update_message "Can't clear path while running simulation!");
  L.resident button

(* simulation logic *)

let animate_plane_icon () =
  if length !waypoints = 0 then (
    update_message "No waypoints to animate through!";
    Printf.printf "No waypoints to animate through!\n";
    flush stdout)
  else (
    simulation_running := true;

    reset_waypoint_statuses ();

    (* initialize plane *)
    if not !plane_initialized then (
      plane_ref :=
        plane_icon
          (fst (coords (get 0 !waypoints)), snd (coords (get 0 !waypoints)));
      plane_initialized := true;
      update_map_layout ());

    let wp_coords = Array.of_list (List.map coords (path_to_list !waypoints)) in

    (* gets coordinates of plane icon for centering *)
    let plane_width = 50 in
    let plane_height = 50 in
    let half_w = plane_width / 2 in
    let half_h = plane_height / 2 in

    (* animation logic, a recursive function based on the next waypoint *)
    let rec animate_path idx =
      if idx = 0 then (
        update_wp_status idx Done;
        update_wp_table ());
      if idx < Array.length wp_coords - 1 then (
        if idx > 0 then update_wp_status idx Done;

        update_wp_status (idx + 1) Pending;
        update_wp_table ();
        update_message (Printf.sprintf "Plane reached waypoint %d!" (idx + 1));

        (* Get the current and next waypoint coordinates *)
        let current_x, current_y = wp_coords.(idx) in
        let next_x, next_y = wp_coords.(idx + 1) in

        (* Adjust positions for plane icon centering *)
        let current_x_adj = current_x - half_w in
        let current_y_adj = current_y - half_h in
        let next_x_adj = next_x - half_w in
        let next_y_adj = next_y - half_h in

        (* compute angle to the next waypoint *)
        let dx = float_of_int (next_x - current_x) in
        let dy = float_of_int (next_y - current_y) in
        let angle_radians = atan2 dy dx in
        let angle_degrees = (angle_radians *. 180.0 /. Float.pi) +. 155.0 in

        (* rotate the plane slowly before moving to the next waypoint *)
        L.rotate ~duration:3 ~angle:angle_degrees !plane_ref;

        (* synchronization for the x and y animations finishing *)
        let finished_count = ref 0 in
        (* callback function necessary for bogue animations *)
        let on_end () =
          incr finished_count;
          if !finished_count = 1 then (
            Printf.printf "Plane reached waypoint %d at (%d, %d)!\n" (idx + 1)
              next_x next_y;
            flush stdout;

            (* once reaching the next waypoint, mark it as Done *)
            update_wp_status (idx + 1) Done;
            update_wp_table ();

            (* animate toward the next waypoint *)
            animate_path (idx + 1))
        in

        (* animate the plane movement *)
        let x_anim =
          Avar.fromto_unif
            ~duration:(distance (current_x, current_y) (next_x, next_y) * !speed)
            ~ending:on_end current_x_adj next_x_adj
        in
        let y_anim =
          Avar.fromto_unif
            ~duration:(distance (current_x, current_y) (next_x, next_y) * !speed)
            ~ending:on_end current_y_adj next_y_adj
        in

        L.animate_x !plane_ref x_anim;
        L.animate_y !plane_ref y_anim)
      else (
        update_wp_status idx Done;
        update_wp_table ();
        update_message "Simulation complete!";
        update_map_layout ();
        Printf.printf "Simulation complete!\n";
        flush stdout;
        simulation_running := false)
    in
    (* starting animation logic *)
    Printf.printf "Simulation started!\n";
    flush stdout;
    animate_path 0)

let start_simulation_button =
  let button = W.button "Start Simulation" in
  W.on_click button ~click:(fun _ ->
      animate_plane_icon ();
      plane_initialized := false);
  L.resident button

(* reset plane button functions*)

let set_plane_icon () =
  if !simulation_running then update_message "Simulation is still running!"
  else (
    plane_ref := plane_icon (150 + !offset_x, 300);
    update_message "Reset Plane Complete!";
    update_map_layout ())

let set_plane_icon_button =
  let button = W.button "Reset Plane" in
  W.on_click button ~click:(fun _ -> set_plane_icon ());
  L.resident button

(* change waypoint UI functionality *)

let wp_entry = W.text_input ~prompt:"WP Name" ()
let x_entry = W.text_input ~prompt:"x" ()
let y_entry = W.text_input ~prompt:"y" ()
let name_entry = W.text_input ~prompt:"New Name" ()

let enter_button =
  let button = W.button "Enter" in
  W.on_click button ~click:(fun _ ->
      if !simulation_running then
        update_message "Can't change waypoint while running simulation!"
      else
        try
          let wp_input = W.get_text wp_entry in
          let x_input =
            let x =
              try int_of_string (W.get_text x_entry)
              with _ -> failwith "x must be an integer."
            in
            if x < 0 || x > 1000 then
              failwith "x must be an integer between 0 and 1000."
            else x
          in
          let y_input =
            let y =
              try int_of_string (W.get_text y_entry)
              with _ -> failwith "y must be an integer."
            in
            if y < 0 || y > 1000 then
              failwith "y must be an integer between 0 and 1000."
            else y
          in
          let name_input =
            let name = W.get_text name_entry in
            if name = "" then failwith "Name must not be empty."
            else if name <> wp_input && contains_name name !waypoints then
              failwith "Name must be unique."
            else name
          in

          try
            let wp_idx = ref None in
            Array.iteri
              (fun i wp -> if name wp = wp_input then wp_idx := Some i)
              !waypoints;

            match !wp_idx with
            | Some idx ->
                let wp = get idx !waypoints in
                set_name wp name_input;
                set_coords wp (x_input, y_input);

                update_message
                  (Printf.sprintf "Updated waypoint %s to (%d, %d) with name %s"
                     wp_input x_input y_input name_input);

                update_wp_table ();
                update_map_layout ()
            | None ->
                update_message
                  (Printf.sprintf "Waypoint with name %s not found!" wp_input)
          with _ ->
            update_message "Invalid input. Please check name and coordinates."
        with Failure m -> update_message m);
  L.resident button

(** app setup *)
let init_app () =
  update_map !map_file;

  let map_combined = L.superpose [ !map; !plane_ref ] in
  let map_layout = L.flat ~margins:0 [ map_combined ] in
  map_layout_ref := Some map_layout;

  let map_scroll = L.make_clip ~w:900 ~h:600 map_layout in
  map_scroll_ref := Some map_scroll;

  (* let wp_table_super = L.superpose [ L.make_clip ~h:25 !wp_table ] in *)
  let wp_table_super = L.superpose [ !wp_table ] in
  wp_table_ref := Some wp_table_super;

  let map_width = fst (W.size (L.widget !map)) in
  let slider_max = if map_width > 900 then map_width - 900 else 0 in

  let slider =
    W.slider_with_action ~value:0 ~length:900 ~thickness:20 ~step:1
      ~kind:Slider.Horizontal
      ~action:(fun value ->
        if not !simulation_running then (
          offset_x := -value;
          apply_offset ())
        else update_message "Can't change map while running simulation!")
      slider_max
  in

  let slider_layout = L.resident slider in

  let map_with_slider = L.tower [ map_scroll; slider_layout ] in

  let sidebar =
    L.tower
      [
        map_menu;
        speed_slider;
        L.flat
          [ clear_path_button; start_simulation_button; set_plane_icon_button ];
        L.resident ~w:300 (W.label "Messages:");
        L.resident ~w:300 !message_label;
        L.resident (W.label "Change Waypoint:");
        L.flat
          [
            L.resident ~w:80 wp_entry;
            L.resident ~w:30 x_entry;
            L.resident ~w:30 y_entry;
            L.resident ~w:80 name_entry;
            enter_button;
          ];
        wp_table_super;
      ]
  in
  let main_layout = L.flat ~sep:20 [ sidebar; map_with_slider ] in
  Bogue.of_layout main_layout

let () =
  let gcs = init_app () in
  Bogue.run gcs
