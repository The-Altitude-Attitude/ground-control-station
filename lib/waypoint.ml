type status =
  | ToDo
  | Pending
  | Done

type waypoint = {
  name : string;
  coords : int * int;
  mutable status : status;
}

type path = waypoint array

exception Empty
exception NotFound
exception OutOfBounds

let create_wp name coords = { name; coords; status = ToDo }
let name wp = wp.name
let coords wp = wp.coords
let status wp = wp.status
let set_status wp status = wp.status <- status
let empty = [||]
let is_empty p = p = [||]
let length p = Array.length p
let contains wp p = Array.mem wp p
let get i p = Array.get p i

let get_index wp p =
  match Array.find_index (fun x -> x = wp) p with
  | None -> raise NotFound
  | Some i -> i

let append wp p =
  let p_len = Array.length p in
  let new_p = Array.make (p_len + 1) wp in
  for i = 0 to p_len - 1 do
    new_p.(i) <- p.(i)
  done;
  new_p

let insert wp i p =
  let p_len = Array.length p in
  if i > p_len then raise OutOfBounds
  else if i = 0 then Array.append [| wp |] p
  else if i = p_len then append wp p
  else
    Array.append (Array.sub p 0 i)
      (Array.append [| wp |] (Array.sub p i (p_len - i)))

let remove i p =
  let p_len = Array.length p in
  if is_empty p then raise Empty
  else if i > p_len - 1 then raise OutOfBounds
  else if i = 0 then Array.sub p 1 (p_len - 1)
  else if i = p_len - 1 then Array.sub p 0 (p_len - 1)
  else Array.append (Array.sub p 0 i) (Array.sub p (i + 1) (p_len - i - 1))

let path_to_list p = Array.to_list p

let distance (x0, y0) (x1, y1) =
  int_of_float
    (sqrt (float_of_int (((x1 - x0) * (x1 - x0)) + ((y1 - y0) * (y1 - y0)))))
