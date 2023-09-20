let plus_one_match x =
  match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 6
  | _ -> x + 1
let plus_one_if x =
  if      x = 0 then 1
  else if x = 1 then 2
  else if x = 2 then 3
  else if x = 3 then 4
  else if x = 4 then 5
  else if x = 5 then 6
  else x + 1

open Core_bench

let bench_test () = [ Bench.Test.create ~name:"plus_one_match" (fun () ->
          plus_one_match 10)
    ; Bench.Test.create ~name:"plus_one_if" (fun () ->
          plus_one_if 10) ]
    |> Bench.bench

let rec sum l =
    match l with
    | [] -> 0
    | hd :: tl -> hd + sum tl
  
open Base    
let rec sum_if l =
  if List.is_empty l then 0
  else List.hd_exn l + sum_if (List.tl_exn l)

let bench_test2 () =  
  let numbers = List.range 0 1000 in
  [ Bench.Test.create ~name:"sum_if" (fun () -> sum_if numbers)
  ; Bench.Test.create ~name:"sum"    (fun () -> sum numbers) ]
  |> Bench.bench  

  let is_ocaml_source s =
    match String.rsplit2 s ~on:'.' with
    | Some (_,("ml"|"mli")) -> true
    | _ -> false

let (ml_files, other_files) = 
  List.partition_tf ["foo.c"; "foo.ml"; "bar.ml"; "bar.mli"] 
  ~f:is_ocaml_source   

module Sys = Core.Sys
module Filename = Core.Filename
 let rec ls_rec s =
    if Sys_unix.is_file_exn ~follow_symlinks:true s
    then [s]
    else
      Sys_unix.ls_dir s
      |> List.concat_map ~f:(fun sub -> ls_rec (Filename.concat s sub))


let max_widths header rows =
  let lengths l = List.map ~f:String.length l in
    List.fold rows
      ~init:(lengths header)
      ~f:(fun acc row -> 
        List.map2_exn ~f:max acc (lengths row))
let render_separator widths =
  let pieces = List.map widths ~f: (fun w -> String.make w '-') in
  "|-" ^ String.concat ~sep:"-+-" pieces ^ "-|"

let pad s len = 
  s ^ String.make (len - String.length s) ' '

let render_row row widths = 
  let padded = List.map2_exn row widths ~f:pad in
"| " ^String.concat ~sep:" | " padded ^ " |"


let render_table header rows = 
  let widths = max_widths header rows in
  String.concat ~sep:"\n" (
    render_row header widths
    :: render_separator widths
    :: List.map rows ~f:(fun row -> render_row row widths)
  )

(* open Base.Poly *)

let render_test () =  Stdio.print_endline
  (render_table
     ["language";"architect";"first release"]
     [ ["Lisp" ;"John McCarthy" ;"1958"] ;
       ["C"    ;"Dennis Ritchie";"1969"] ;
       ["ML"   ;"Robin Milner"  ;"1973"] ;
       ["OCaml";"Xavier Leroy"  ;"1996"] ;
])  