(* value restriction *)
let map_id = List.map (fun x -> x)
let lst1 = map_id [1; 2]

(* error *)
(* let lst2 = map_id ['a'; 'b'] *)
let lst2 = List.map (fun x -> x) ['a'; 'b']


(* unallowed nested quatification *)
(* error *)
(* let foo f = (
   f [1; 2; 3], f [true; false]) *)

open Base

let ratio x y =
  Float.of_int x /. Float.of_int y

let ratio' x y = 
  let open Float.O in
  of_int x / of_float y
  
let ratio'' x y =
  Float.O.(of_int x / of_int y)

let test x y =
  x /% y 

let sum_if_true test first second =
  (if test first then first else 0) 
  +
  (if test second then second else 0)

let even x =
  x % 2 = 0
  
let sum_if_true' (test : int -> bool) (first : int) (second : int) : int =
  (if test first then first else 0) + (if test second then second else 0)

let first_if_true test x y =
  if test x then x else y


let distance (x1, x2) (y1, y2) =
  Float.sqrt ((x1 -. x2) **. 2. +. (y1 -. y2) **. 2.)

let map_id = List.map ~f:(fun x -> x)  
 
let downcase_extention filename =
  match String.rsplit2 filename ~on:'.' with
  | None -> filename
  | Some (base, ext) ->
    base ^ "." ^ String.lowercase ext

(* OCaml stdlib don't support : *)
(* let x = if 2. > 2. then 0 else 1     *)

type 'a rrr = {
  mutable rr: 'a
}

let rrr x = {
  rr = x
}
  
let ra = rrr 1

let ( $! ) x = x.rr

let a = ($!) ra

let ( $:= ) x a  = x.rr <- a


let ( $ ) a = -a
let z = ($) 1


let ( $$$ ) a b c = a + b + c
let zzz =  ($$$) 1 3 4
let zzz' = 3 $$$ 1
let zzz'' = (3 $$$ 1) 3

let ( $$ ) a b = a + b
let zz =  1 $$ 3 