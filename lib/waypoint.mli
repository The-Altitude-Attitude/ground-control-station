type waypoint = { name : string; coords : int * int }
(** The type of a waypoint whose name is type [string] and coords is type [int * int]. *)

type path = waypoint array
(** The type of a path whose elements are waypoints. *)

exception Empty
(** Exception raised when attempting to access a waypoint of an empty path. *)

exception NotFound
(** Exception raised when attempting to access a non-existent waypoint in a path. *)

exception OutOfBounds
(** Exception raised when attempting to access outside of the bounds of the path. *)

val create_wp : string -> int * int -> waypoint
(** [create_wp name coords] is a waypoint with name [name] and coords [coords]. *)

val empty : path
(** [empty] is an empty path. *)

val is_empty : path -> bool
(** [is_empty p] is whether or not path [p] is an empty path. *)

val contains : waypoint -> path -> bool
(** [contains wp p] is whether or not waypoint [wp] is in path [p]. *)

val get : int -> path -> waypoint
(** [get i p] is the waypoint at index [i] in path [p]. *)

val get_index : waypoint -> path -> int
(** [get_index wp p] is the index of the first occurrence of waypoint [wp] in path [p]. *)

val append : waypoint -> path -> path
(** [append wp p] is path [p] with waypoint [wp] appended to the end. *)

val insert : waypoint -> int -> path -> path
(** [insert wp i p] is path [p] with waypoint [wp] inserted into index [i]. *)

val remove : int -> path -> path
(** [remove i p] is path [p] with the waypoint at index [i] removed. *)
