open Base
let s = object
  val mutable v = [0; 2]

  method pop =
    match v with
    | hd :: tl ->
      v <- tl;
      Some hd
    | [] -> None

  method push hd =
    v <- hd :: v
end

let zero = s#pop
let two = s#push
let none = s#push

let () = s#push 1

let stack init = object
  val mutable v = init

  method pop =
    match v with
    | h :: t ->
      v <- t;
      Some h
    | [] -> None

  method push hd =
    v <- hd :: v
end

let s = stack [1; 2; 3; 4]
let one = s#pop


let area sq = sq#width * sq#width
let minimize sq : unit = sq#resize 1 
let limit sq = if (area sq) > 100 then minimize sq

(* let toggle sq b : unit =
  if b then sq#resize `Fullscreen else minimize sq  *)

let area_closed (sq : <width : int>) = sq#width * sq#width
let sq = object
  method width = 30
  method name = "sq"
end
(* let error = area_closed sq *)
let ok = area sq

let print_pop st = Option.iter ~f:(Stdio.printf "Popped: %d\n") st#pop

let test() = print_pop (stack [1; 2; 3; 4])

let array_stack l = object
  val stack = Stack.of_list l
  method pop = Stack.pop stack
end

let test2 () = print_pop (array_stack [1; 2; 3; 4])


let imm_stack init = object
  val v = init

  method pop =
    match v with
    | [] -> None
    | h :: t -> Some (h, {<v = t>})

  method push hd = 
    {< v = hd :: v >}
end

let s = imm_stack [1; 2; 3]

let new_obj = s#push 4
let take_some = function
  | Some x -> x
  | None -> failwith "None"

let (top, new_obj) = take_some (s#pop)
