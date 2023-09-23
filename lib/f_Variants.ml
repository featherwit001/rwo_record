open Base
open Stdio
type basic_color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let basic_color_to_num = function
  | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7

let colored_by_number number text =
  Printf.sprintf "\o033[38;5;%dm%s\x1b[0m" number text
  (* Printf.sprintf "\027[38;5;%dm%s\x1b[0m" number text *)


let blue = colored_by_number (basic_color_to_num Blue) "Blue"

let pb () = print_endline blue

type weight = Regular | Bold

type color =
  | Basic of basic_color
  | RGB of int * int * int (* 6 * 6 * 6 *)
  | Gray of int (* 24 grayscale from 0 to 23*)

let bold_or_not b text = 
  let b = match b with 
    | Bold -> 1
    | Regular -> 0
  in Printf.sprintf "\027[%dm%s\027[0m" b text 

let color_to_num = function
  | Basic (basic_color) -> 
      basic_color_to_num basic_color
  | RGB (r, g, b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i

let color_print ?(b=Regular) color s =
  (colored_by_number (color_to_num color) s)
  |> bold_or_not b
  |> printf "%s\n" 

let prb () = color_print (Basic Red) ~b:Bold "a bold red!" 
let pg4 () = color_print (Gray 12) ~b:Regular "A muted gray...."

type 'a expr = 
  | Base of 'a
  | Const of bool
  | And of 'a expr list
  | Or of 'a expr list
  | Not of 'a expr

type mail_field = To | From | CC | Date | Subject
type mail_predicate = { field: mail_field;
                        contains: string }

let test field contains = Base { field; contains }

let mailexpr = 
And [ Or [ test To "doligez"; test CC "doligez" ];
  test Subject "runtime";]


let rec eval expr base_eval = 
  let eval' expr = eval expr base_eval in
  match expr with
  | Base base -> base_eval base
  | Const bool -> bool
  | And exprs -> List.for_all exprs ~f:eval'
  | Or exprs -> List.exists exprs ~f:eval'
  | Not expr -> not (eval' expr)


(* marked! *)
let and_ l =
  if List.exists l ~f:(function Const false -> true | _ -> false)
  then Const false
  else
    match List.filter l ~f:(function Const true -> false | _ -> true) 
    with
      | [] -> Const true
      | [x] -> x
      | l -> And l 

let or_ l =
  if List.exists l ~f:(function Const true -> true | _ -> false)
  then Const true
  else
    match List.filter l ~f:(function Const false -> false | _ -> true)
    with
      | [] -> Const false
      | [x] -> x
      | l -> Or l
     
let not_ = function
  | Const b -> Const (not b)
  | Not e -> e
  | e -> Not e

let rec simplify = function
  | Base _ | Const _ as x -> x
  | And l -> and_ (List.map ~f:simplify l)
  | Or l -> or_ (List.map ~f:simplify l)
  | Not e -> not_ (simplify e)

let expr_simplified1 = 
  simplify (Not (And [ Or [Base "it's snowing"; Const true];
          Base "it's raining"]))

let expr_simplified2 = 
  simplify (Not (And [ Or [Base "it's snowing"; Const true];
  Not (Not (Base "it's raining"))]))


let three = `Int 3
let four = `Float 4.
let  nan = `Not_a_number

let lst = [three; four; ]

let is_positive = function
  |  `Int i -> i > 0
  | `Float f -> Float.(f > 0.)
  (* | _ -> false *)

let exact = List.filter ~f:is_positive lst

let is_positive = function
  |  `Int i -> Ok (i > 0)
  | `Float f -> Ok (Float.(f > 0.))
  | `Not_a_number -> Error "not a number"

let less = List.filter lst ~f:(fun x -> 
    match is_positive x with Ok b -> b | Error _ -> false ) 


type extended_color =
  | Basic of basic_color * weight  (* basic colors, regular and bold *)
  | RGB   of int * int * int       (* 6x6x6 color space *)
  | Gray  of int                   (* 24 grayscale levels *)
  | RGBA  of int * int * int * int (* 6x6x6x6 color space *)

    
let basic_color_to_int = function
  | `Black -> 0 | `Red     -> 1 | `Green -> 2 | `Yellow -> 3
  | `Blue  -> 4 | `Magenta -> 5 | `Cyan  -> 6 | `White  -> 7

let color_to_int = function
  | `Basic (basic_color,weight) ->
    let base = match weight with `Bold -> 8 | `Regular -> 0 in
    base + basic_color_to_int basic_color
  | `RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | `Gray i -> 232 + i

let extended_color_to_int = function
  | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216 
  | (`Basic _ | `RGB _ | `Gray _ ) as color -> color_to_int color
  (* | color -> color_to_int color  (* error! *) *)



