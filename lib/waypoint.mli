(** The type of a waypoint's status. *)
type status =
  | ToDo
  | Pending
  | Done

type waypoint = {
  mutable name : string;
  mutable coords : int * int;
  mutable status : status;
}
(** The type of a waypoint whose name is type [string], coords is type
    [int * int], and status is of type [status]. *)

type path = waypoint array
(** The type of a path whose elements are waypoints. *)

exception Empty
(** Exception raised when attempting to access a waypoint of an empty path. *)

exception NotFound
(** Exception raised when attempting to access a non-existent waypoint in a
    path. *)

exception OutOfBounds
(** Exception raised when attempting to access outside of the bounds of the
    path. *)

val create_wp : string -> int * int -> waypoint
(** [create_wp name coords] is a waypoint with name [name] and coords [coords]. *)

val name : waypoint -> string
(** [name wp] is the name of waypoint [wp]. *)

val coords : waypoint -> int * int
(** [coords wp] is the coordinates of waypoint [wp]. *)

val status : waypoint -> status
(** [status wp] is the status of waypoint [wp]. *)

val set_name : waypoint -> string -> unit
(** [set_name wp name] sets the name of waypoint [wp] to [name]. *)

val set_coords : waypoint -> int * int -> unit
(** [set_coords wp coords] sets the coords of waypoint [wp] to [coords]. *)

val set_status : waypoint -> status -> unit
(** [set_status wp status] sets the status of waypoint [wp] to status [status]. *)

val empty : path
(** [empty] is an empty path. *)

val is_empty : path -> bool
(** [is_empty p] is whether or not path [p] is an empty path. *)

val length : path -> int
(** [length p] is the length of path [p]. *)

val contains : waypoint -> path -> bool
(** [contains wp p] is whether or not waypoint [wp] is in path [p]. *)

val contains_name : string -> path -> bool
(** [contains_name n p] is whether or not a waypoint with name [n] is in path
    [p]. *)

val get : int -> path -> waypoint
(** [get i p] is the waypoint at index [i] in path [p]. *)

val get_index : waypoint -> path -> int
(** [get_index wp p] is the index of the first occurrence of waypoint [wp] in
    path [p]. *)

val append : waypoint -> path -> path
(** [append wp p] is path [p] with waypoint [wp] appended to the end. *)

val insert : waypoint -> int -> path -> path
(** [insert wp i p] is path [p] with waypoint [wp] inserted into index [i]. *)

val remove : int -> path -> path
(** [remove i p] is path [p] with the waypoint at index [i] removed. *)

val path_to_list : path -> waypoint list
(** [path_to_list p] is a list of waypoints. *)

val distance : int * int -> int * int -> int
(** [distance (x0, y0) (x1, y1)] is the distance between [(x0, y0)] and
    [(x1, y1)]. *)
