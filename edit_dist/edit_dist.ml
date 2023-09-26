let time f =
  let open Core in 
  let start = Time.now() in
  let x = f () in
  let stop = Time.now() in
  Printf.printf "Time: %F ms\n" (Time.diff stop start |> Time.Span.to_ms);
  x

(* index i and j from 1, so string index have to minus 1 *)
let edit_dist a b =
  let alen = String.length a in
  let blen = String.length b in
  let f = Array.make_matrix (alen + 1) (blen + 1) 0 in
  for i = 0 to blen do f.(0).(i) <- i done;
  for i = 0 to alen do f.(i).(0) <- i done;
  for i = 1 to alen do
    for j = 1 to blen do
      f.(i).(j) <- min (f.(i - 1).(j) + 1) (f.(i).(j - 1) + 1);
      if a.[i - 1] = b.[j - 1] then  f.(i).(j) <- min f.(i).(j) f.(i - 1).(j - 1) 
      else f.(i).(j) <- min f.(i).(j) (f.(i - 1).(j - 1) + 1)
    done
  done;
  f.(alen).(blen)



let test () =
  let a = "AGTCTGACGC" in
  let b = "AGTAAGTAGGC" in
  let edit_distance = edit_dist a b in
  assert (edit_distance = 4)

open Base
let rec edit_distance s t =
  match String.length s, String.length t with
  | (0,x) | (x,0) -> x
  | (len_s,len_t) ->
    let s' = String.drop_suffix s 1 in
    let t' = String.drop_suffix t 1 in
    let cost_to_drop_both =
      if Char.(=) s.[len_s - 1] t.[len_t - 1] then 0 else 1
    in
    List.reduce_exn ~f:Int.min
      [ edit_distance s' t  + 1
      ; edit_distance s  t' + 1
      ; edit_distance s' t' + cost_to_drop_both
      ]

let fib_norec fib i =
  if i <= 1 then i
  else fib (i - 1) + fib (i - 2)  

let make_rec f_norec =
  let rec f x = f_norec f x in
  f

let fib = make_rec fib_norec


let memo_rec' m f =
  let h = Hashtbl.create m in
  let rec g x = 
    Hashtbl.find_or_add h x ~default:(fun () -> f g x)
  in g

let memoize m f =
  let memo_table = Hashtbl.create m in
  (fun x ->
     Hashtbl.find_or_add memo_table x ~default:(fun () -> f x))

let memo_rec m f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize m (fun x -> f_norec !fref x) in
  fref := f;
  f x


open Ppx_jane
module String_pair = struct
  type t = string * string [@@deriving sexp_of, hash, compare]
end

(* in the utop, #require "ppx_jane";; could evaluate String_pair ahead of using *)
let edit_distance = memo_rec (module String_pair)
    (fun edit_distance (s,t) ->
       match String.length s, String.length t with
       | (0,x) | (x,0) -> x
       | (len_s,len_t) ->
         let s' = String.drop_suffix s 1 in
         let t' = String.drop_suffix t 1 in
         let cost_to_drop_both =
           if Char.(=) s.[len_s - 1] t.[len_t - 1] then 0 else 1
         in
         List.reduce_exn ~f:Int.min
           [ edit_distance (s',t ) + 1
           ; edit_distance (s ,t') + 1
           ; edit_distance (s',t') + cost_to_drop_both
  ])

