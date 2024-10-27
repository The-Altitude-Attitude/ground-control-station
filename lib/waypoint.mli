type waypoint = { name : string; coords : int * int }
type path = waypoint array

exception Empty
exception NotFound

val create_wp : string -> int * int -> waypoint (*creat -> create*)
val empty : 'a array
val is_empty : 'a array -> bool
val append : 'a -> 'a array -> 'a array
val remove : 'a -> 'a array -> 'a array
