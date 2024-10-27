type waypoint = { name : string; coords : int * int }
type path = waypoint array

exception Empty
exception NotFound

let creat_wp name coords = { name; coords }
let empty = [||]
let is_empty p = p = [||]

let append wp p =
  let p_len = Array.length p in
  let new_p = Array.make (p_len + 1) wp in
  for i = 0 to p_len - 1 do
    new_p.(i) <- p.(i)
  done;
  new_p

let remove wp p =
  if is_empty p then raise Empty
  else
    let index = Array.find_index (fun x -> x = wp) p in
    match index with
    | None -> raise NotFound
    | Some i ->
        Array.append (Array.sub p 0 i)
          (Array.sub p (i + 1) (Array.length p - i - 1))
