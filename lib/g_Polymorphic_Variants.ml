open Base

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





type basic_color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

type weight = Regular | Bold

type color =
  | Basic of basic_color * weight (* basic colors, regular and bold *)
  | RGB   of int * int * int      (* 6x6x6 color cube *)
  | Gray  of int                  (* 24 grayscale levels *)

let basic_color_to_int = function
  | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7  

let color_to_int = function
  | Basic (basic_color,weight) ->
    let base = match weight with Bold -> 8 | Regular -> 0 in
    base + basic_color_to_int basic_color
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i


type extended_color =
  | Basic of basic_color * weight  (* basic colors, regular and bold *)
  | RGB   of int * int * int       (* 6x6x6 color space *)
  | Gray  of int                   (* 24 grayscale levels *)
  | RGBA  of int * int * int * int (* 6x6x6x6 color space *)

(* let extended_color_to_int = function
  | RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216



  | (Basic _ | RGB _ | Gray _) as color -> color_to_int color  *)
  (*error on color for color and extended_color are distinct and unrelated type *)

  
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
  (* | color -> color_to_int color  *)
  (* error on catch-all case *)


(* improved version 2 *)

type basic_color' = 
  [ `Black | `Blue | `Cyan | `Green 
  |`Magenta | `Red | `White | `Yellow ]

type color' = 
  [ `Basic of basic_color' * [ `Bold  | `Regular ]
  | `Gray of int
  | `RGB of int * int * int ]

type extended_color' =
  [ color'
  | `RGBA of int * int * int * int ]


let basic_color_to_int' = function
  | `Black -> 0 | `Red     -> 1 | `Green -> 2 | `Yellow -> 3
  | `Blue  -> 4 | `Magenta -> 5 | `Cyan  -> 6 | `White  -> 7

let color_to_int' = function
  | `Basic (basic_color,weight) ->
    let base = match weight with `Bold -> 8 | `Regular -> 0 in
    base + basic_color_to_int' basic_color
  | `RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | `Gray i -> 232 + i

let extended_color_to_int' : extended_color' -> int = function
  | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216 
  |  #color' as color -> color_to_int' color
  