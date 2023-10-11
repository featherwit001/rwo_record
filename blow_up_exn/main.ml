open Base
open Stdio
exception Empty_list

let list_max = function
  | [] -> raise Empty_list
  | hd :: tl -> List.fold tl ~init:hd ~f:(Int.max)

(** exception goes up the stack of function call and shows backtrace *)
let () =
  printf "%d\n" (list_max [1;2;3]);
  printf "%d\n" (list_max [])
(* OCAMLRUNPARAM=b=0 dune exec -- ./blow_up.exe *)
