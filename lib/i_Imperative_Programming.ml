open Base
open Stdio
let x = List.find_map 

let y = Array.blit 

let b = Bytes.of_string "foobar"
let () = Bytes.set b 0 (Char.uppercase (Bytes.get b 0))
let b = Bytes.to_string b


type 'a sequence = Cons of 'a * (unit -> 'a sequence)
let rec sum (Cons(h1, t1)) (Cons(h2, t2)) =
  Cons(h1 + h2, fun() -> sum (t1()) (t2()))
let hd (Cons(h, _)) = h
let tl (Cons(_, t)) = t()

let rec fibs = 
  Cons (1 , fun () ->
    Cons (1, fun () ->
      sum fibs (tl fibs)))


let v = lazy (print_endline "performing lazy computation"; Float.sqrt 16.)

let lazy_test () = 
  let res1 = Lazy.force v in
  let res2 = Lazy.force v in
  (res1, res2)

type 'a lazy_state =
  | Delayed of (unit -> 'a)
  | Value of 'a
  | Exn of exn

type 'a lazy_value = {mutable state : 'a lazy_state}

let our_lazy expr = {state = Delayed (fun () -> expr)}
let our_force l = 
  match l.state with
  | Value v -> v
  | Exn e -> raise e
  | Delayed f -> 
    try let v = f() in l.state <- Value v; v 
    with e -> l.state <- Exn e; Caml.raise_notrace e


let our_lazy_v = 
  our_lazy (print_endline "performing lazy computation"; Float.sqrt 16.)
let lazy_test () =
  let v = our_force our_lazy_v in
  let x = our_force our_lazy_v in
  (v, x)

let res =   
let x = lazy(Float.sin 120.) in
let y = lazy(Float.sin 75.) in
let z = lazy(Float.sin 128.) in
List.exists ~f:(fun x -> Float.O.(Lazy.force x < 0.)) [x; y; z]


(* let remember =
  let cache = ref None in
  (fun x ->
     match !cache with
     | Some y -> y
     | None -> cache := Some x; x)

    
let cache = ref None 

let id x = x
let f = fun x -> [x; x]
let idf = id (fun x -> [x; x])

let lls = List.init

let list_init_10 = List.init 10
let list_init_10 ~f = List.init 10 ~f


let idemptylst = id []
let idemptyarr = id [||] *)
