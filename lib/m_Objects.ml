open Base
(* 忽略val，只显示method的类型，并且method 的输入中有object中的val也省略 *)
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

(* infer that the types of object are conflictive  *)
(* let toggle sq b : unit =
  if b then sq#resize `Fullscreen else minimize sq  *)

(* annotate the type of object explicitly *)
let area_closed (sq : <width : int;..>) = sq#width * sq#width
let sq = object
  method width = 30
  method name = "sq"
end

(* let error = area_closed sq *)
let area_not_closed (sq : <width : int; ..>) = sq#width * sq#width
let ok = area_not_closed sq
let ok = area sq

let print_pop st = Option.iter ~f:(Stdio.printf "Popped: %d\n") st#pop

let test() = print_pop (stack [1; 2; 3; 4])

let array_stack l = object
  val stack = Stack.of_list l
  method pop = Stack.pop stack
end

let test2 () = print_pop (array_stack [1; 2; 3; 4])

(* immutable val using {< v = x >} to replace the object val
   but keep the methods unchanged
   Remember: return the new object 
   *)
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


(* subtype *)

type shape = < area : float >

let shape f : shape = object 
  method area = f 
end

type square = < area : float; width : int >

let square w = object
  method area = Float.of_int (w * w)
  method width = w
end


type circle = < area : float; radius : int >

let circle r = object 
  method area = 3.14 *. (Float.of_int r) **. 2.0 
  method radius = r
end

let coin = object 
  method shape = circle 5
  method color = "silver"
end

let map = object 
  method shape = square 10 
end

(* depth Subtype 
types :
  square :> shape
  circle :> shape   

and 
objects :
  coin has method shape which returns an object that is a subtype of shape   
  map has method shape which returns an object that is a subtype of shape

  coin : < m : t2; ..> 
  map  : < m : t3; ..>
  item : < m : t1 >
  t2 :> t1 and t3 :> t1
  so
  coin :> item and map :> item 
*)
type item = < shape : shape >

let items = [(coin :> item); (map :> item)]



(* similarly *)
type num = [ `Int of int | `Float of float ]

type const = [ num | `String of string ]

let n : num = `Int 3
let c : const = (n :> const)



let squares : square list = [ square 10; square 20 ]
(* 'a list is covariant *)
let shapes : shape list = (squares :> shape list)


let square_array : square array = [| square 10; square 20 |]

(* 'a array is invariant *)
(* let shape_array : shape array = (square_array :> shape array) *)


let shape_to_string : shape -> string =
  fun s -> Printf.sprintf "Shape(%F)" s#area


(* function type can be contravariant
  where requires subtype can be filled with father type 
  arguments are cotravariant. return type is covariant *)
let square_to_string : square -> string = 
  (shape_to_string :> square -> string)



module Either = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
  let left x = Left x
  let right x = Right x
end


let left_square = Either.left (square 40)
(* covariant subtype can be used where father type is required. *)
let ls = (left_square :> (shape,_) Either.t)

(* invariant *)
module Abs_either : sig
  type ('a, 'b) t
  val left: 'a -> ('a, 'b) t
  val right: 'b -> ('a, 'b) t
end = Either

(* invariant so that covariant is not allowed. *)
(* let abs_ls_err = ((Abs_either.left (square 40)) :> (shape, _) Abs_either.t) *)



(* add Variant Annotations
   + for covariance
   - for contravariance
   none annotation for invariance  *)
module Var_either : sig
  type (+'a, +'b) t
  val left: 'a -> ('a, 'b) t
  val right: 'b -> ('a, 'b) t
end = Either 

let var_ls_ok = ((Var_either.left (square 40)) :> (shape, _) Var_either.t)


type 'a stack = < pop: 'a option; push: 'a -> unit >
let square_stack : square stack = stack [square 30; square 10]
let circle_stack : circle stack = stack [circle 20; circle 40]

let total_area (shape_stack: shape stack list) =
  let stack_area acc st =
    let rec loop acc =
      match st#pop with
      | None -> acc 
      | Some s -> loop (acc +. s#area)
    in
    loop acc
  in List.fold ~init:0.0 ~f:stack_area shape_stack

(* push method *)
(* let err = total_area [(square_stack :> shape stack); (circle_stack :> shape stack)] *)


type 'a readonly_stack = < pop : 'a option >

let total_area' (shape_stack: shape readonly_stack list) =
  let stack_area acc st =
    let rec loop acc =
      match st#pop with
      | None -> acc 
      | Some s -> loop (acc +. s#area)
    in
    loop acc
  in List.fold ~init:0.0 ~f:stack_area shape_stack

let res = total_area' [(square_stack :> shape readonly_stack); 
                       (circle_stack :> shape readonly_stack)]

(* narrowing is not supported in OCaml *)

type shape' = Circle of { radius : int } | Line of { length : int }

let is_barbell = function
  | [Circle {radius = r1}; Line _; Circle {radius = r2}] when r1 = r2 -> true
  | _ -> false


type shape'' = < variant : repr >
and circle' = < variant : repr; radius : int >
and line   = < variant : repr; length : int >
and repr = 
  | Circle of circle'
  | Line of line

(* do not provide much value here *)
let is_barbell = function
  | [s1; s2; s3] ->
    (match s1#variant, s2#variant, s3#variant with
    | Circle c1, Line _, Circle c2 when c1#radius = c2#radius -> true 
    | _ -> false)
  | _ -> false


(* row polymorphism *)
let remove_large l =
  List.filter ~f:(fun s -> Float.(s#area <= 100.)) l

let squares : < area : float; width : int > list = [square 5; square 15; square 10]

let res = remove_large squares



let remove_large' (l : < area : float > list ) =
  List.filter ~f:(fun s -> Float.(s#area <= 100.)) l

let squares : < area : float; width : int > list = [square 5; square 15; square 10]

let res = remove_large' (squares :> < area : float > list)


(* row polymorphism does not work in 
   creating a container with heterogeneous elements
   and
  store different types of object in same reference *)
(* but subtyping works well *)

(* let hlist: < area : float; ..> list = [square 10; circle 30] *)


let hlist : shape list = [(square 10 :> shape); (circle 30 :> shape)]


(* let shape_ref : < area : float; ..> ref = ref (square 10)

let () = shape_ref := circle 20  *)


let shape_ref : shape ref = ref (square 40 :> shape)

let () = shape_ref := (circle 20 :> shape)




