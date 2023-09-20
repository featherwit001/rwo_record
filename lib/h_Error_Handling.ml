open Base

let find_mismatches table1 table2 =
  Hashtbl.fold table1 ~init:[] ~f:(fun ~key ~data mismatches ->
    match Hashtbl.find table2 key with
    | Some data' when data' <> data -> key :: mismatches
    | _ -> mismatches
  )


let err1 = Error (Error.of_string "failed")

let float_of_string s =
  Or_error.try_with (fun () -> Float.of_string s)

let err2 = Error.create "Unexpected character" 'c' Char.sexp_of_t 

let err3 = Error.t_of_sexp 
  [%sexp ("List is too long", [1;2;3] : string * int list)]

let err4 = Error.tag
    (Error.of_list [ Error.of_string "Your tires were slashed";
                     Error.of_string "Your windshield was smashed" ])
    ~tag:"over the weekend"


let a = "foo" and b = ("foo", [3; 4])
let err5 = Or_error.error_s
  [%message "Something went wrong" (a:string) (b: string * int list)]

open Ppx_let
let compute_bounds ~compare list =
  let open Option.Let_syntax in
  let sorted = List.sort ~compare list in
  let%bind first = List.hd sorted in
  let%bind last = List.last sorted in
  Some (first, last)
(** let%bind x = expr1 in expr2 
    equals to expr1 >>= fun x -> expr2 *)

let res = Option.both


type 'a bounds = {lower: 'a ; upper: 'a} [@@deriving sexp]

exception Crossed_bounds of int bounds [@@deriving  sexp]
let exn1 = Crossed_bounds {lower = 10; upper = 0}
(* if we don't use sexp, the output will be (_) *)

let merge_lists xs ys ~f =
  let rec loop xs ys =
    match xs,ys with
    | [],[] -> []
    | x::xs, y::ys -> f x y :: loop xs ys
    | _ -> assert false
  in
  loop xs ys

let assertfail () = merge_lists [1;2;3] [-1] ~f:(+)


exception Key_not_found of string

let rec find_exn alist key = match alist with
  | [] -> raise (Key_not_found key)
  | (key',data) :: tl -> if String.(=) key key' then data else find_exn tl key


let lookup_weight ~compute_weight alist key = 
  match find_exn alist key with
  | exception _ -> 0.
  | data -> compute_weight data
  (* catch excption with match-with sentences directly *)
  (* the better resolution is to use exception-free funciton
     propagate and handle errors with option and result *)

let err5 = Or_error.try_with 
let err5 = Or_error.ok_exn



     