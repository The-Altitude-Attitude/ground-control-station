(* let () = print_endline "Hello, World!" *)
open Bogue

let () =
  Widget.label "Hello world" |> Layout.resident |> Bogue.of_layout |> Bogue.run
