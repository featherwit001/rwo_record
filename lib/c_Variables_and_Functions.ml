open Base
let x = String.split ~on:',' ""

let split_gen ~on =
  let is_delim =
    match on with
    | `char c' -> fun c -> Char.equal c c'
    | `char_list l -> fun c -> List.mem l c ~equal:(fun x y -> Char.to_int x = Char.to_int y )
  in
  is_delim

(* wrapped with [ ], and prefix their name with backquote character ` *)
type polyvariant = [ `on | `off ]

let f = function `on -> 1 | `off -> 0 | `number n -> n 

let split_cases = function
    | `Nil | `Cons _ as x -> `A x
    | `Snoc _ as x -> `B x


let ratio ~num ~denom = 
  Float.of_int num /. Float.of_int denom

let num = 3 
let denom = 4 
(* label punning *)
let r = ratio ~num ~denom

(* explicitly *)
let concat ~sep a b =
  let sep = match sep with  None -> "" | Some s -> s in
  a ^ sep ^ b

let concat ?sep a b =
  let sep = match sep with  None -> "" | Some s -> s in
  a ^ sep ^ b
(* or *) 
let concat ?(sep = "") a b = a ^ sep ^ b


let f a b ~c ?d e = a ^ b ^ c ^ e ^ (match d with None -> "" | Some s -> s)

(* ?d:string has been erased.  *)
let partial_applicate_f = f "1" "2" "3"
(* ?d:string still exists *)
let partial_applicate_f = f "1" "2"
