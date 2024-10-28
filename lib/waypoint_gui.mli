open Waypoint
open Bogue

val wp_widget : waypoint -> Widget.t
(** [wp_widget wp] creates a widget to represent waypoint [wp] *)

val wp_layout : waypoint -> Layout.t
(** [wp_layout wp] creates a positioned layout for waypoint [wp] *)

val wp_map_render : Layout.t -> waypoint list -> Layout.t
(** [wp_map_render map wps] renders waypoints [wps] on [map] layout *)

val wp_layout_map : string -> Layout.t
(** [wp_layout_map path] creates clickable map layout from image at [path] *)

val add_waypoint : int -> int -> unit
(** [add_waypoint x y] adds a waypoint at coordinates [(x,y)] *)
