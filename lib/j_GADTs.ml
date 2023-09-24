open Base

type value' =
  | Int of int
  | Bool of bool

type expr' =
  | Value of value'
  | Eq of expr' * expr'
  | Plus of expr' * expr'
  | If of expr' * expr' * expr'
  
exception Ill_typed

let rec eval expr =
  match expr with
  | Value v -> v
  | If (c, t, e) ->
    (match eval c with
     | Bool b -> if b then eval t else eval e
     | Int _ -> raise Ill_typed)
  | Eq (x, y) ->
    (match eval x, eval y with
     | Bool _, _ | _, Bool _ -> raise Ill_typed
     | Int f1, Int f2 -> Bool (f1 = f2))
  | Plus (x, y) ->
    (match eval x, eval y with
     | Bool _, _ | _, Bool _ -> raise Ill_typed
     | Int f1, Int f2 -> Int (f1 + f2))

let i x = Value (Int x)
and b x = Value (Bool x)
and (+:) x y = Plus (x,y)
(* type unsafe *)
let will_raise () = eval (i 3 +: b false);



module type Typesafe_lang_sig = sig
  type 'a t

  (** functions for constructing expressions *)

  val int : int -> int t
  val bool : bool -> bool t
  val if_ : bool t -> 'a t -> 'a t -> 'a t
  val eq : 'a t -> 'a t -> bool t
  val plus : int t -> int t -> int t

  (** Evaluation functions *)

  val int_eval : int t -> int
  val bool_eval : bool t -> bool
end

module Typesafe_lang : Typesafe_lang_sig = struct
  type 'a t = expr'

  let int x = Value (Int x)
  let bool x = Value (Bool x)
  let if_ c t e = If (c, t, e)
  let eq x y = Eq (x, y)
  let plus x y = Plus (x, y)

  let int_eval expr =
    match eval expr with
    | Int x -> x
    | Bool _ -> raise Ill_typed

  let bool_eval expr =
    match eval expr with
    | Bool x -> x
    | Int _ -> raise Ill_typed
end

(* It will be rejected by the type system *)
(* let safe_type_test () = Typesafe_lang.(plus (int 3) (bool false)) *)

let int = Typesafe_lang.int
let bool = Typesafe_lang.bool
let plus = Typesafe_lang.plus


(* wrong type  *)
let equal = Typesafe_lang.eq
let boolexpr = Typesafe_lang.(eq (bool true) (bool true))
let will_raise2 () = Typesafe_lang.bool_eval boolexpr


(* first GADT *)
type _ value =
  | Int : int -> int value
  | Bool: bool -> bool value

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr  
  
let i x = Value (Int x)
and b x = Value (Bool x)
and ( +: ) x y =  Plus (x, y)


let rec eval : type a. a expr -> a = function
  | Value v -> eval_value v
  | If (g, t, f) -> if eval g then eval t else eval f
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y
and eval_value : type a. a value -> a = function
  | Int x -> x
  | Bool x -> x


let eval_value (type a) (v : a value) : a =
  match v with
  | Int x -> x
  | Bool x -> x

  (* error! *)
(* let rec eval (type a) (e : a expr) : a =
  match e with
  | Value v -> eval_value v
  | If (c, t, e) -> if eval c then eval t else eval e
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y *)


let rec eval: 'a. 'a expr -> 'a =
  fun (type a) (x : a expr) ->
    match x with
    | Value v -> eval_value v
    | If (c, t, e) -> if eval c then eval t else eval e
    | Eq (x, y) -> eval x = eval y
    | Plus (x, y) -> eval x + eval y
  
(* syntactic sugar *)
let rec eval: type a. a expr -> a = function
  | Value v -> eval_value v
  | If (c, t, e) -> if eval c then eval t else eval e
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y


(* module If_not_found = struct
  type 'a t =
    | Raise
    | Return_none
    | Default_to of 'a
end   *)

type 'a t1 =
  | One of int
  | Two of bool * 'a
  | Thr of char * 'a t1
  | For of float * 'a t1

type ('a, 'b) t2 =
  | A : 'a *'b -> ('a, 'b array) t2
  | B : 'a -> ('a, 'a list) t2
  | C : 'b -> ('b, 'b option)t2


(* the first is input elt type and the second is outuput type*)
module If_not_found = struct
  (* type ('a, 'b) t = *)
  type (_, _) t = 
    | Raise : ('a, 'a) t
    | Return_none : ('a, 'a option) t
    | Default_to : 'a -> ('a, 'a) t
end

let rec flexible_find 
  : type a b. f:(a -> bool) -> a list -> (a, b) If_not_found.t -> b =
  fun ~f list if_not_found->
    match list with
    | [] -> 
      (match if_not_found with
      | Raise -> failwith "No matching item found"
      | Return_none -> None
      | Default_to x -> x)
    | hd :: tl ->
      if f hd
      then (
        match if_not_found with
        | Raise -> hd
        | Return_none -> Some hd
        | Default_to _ -> hd)
      else flexible_find ~f tl if_not_found 

let test1 () = flexible_find ~f:(fun x -> x > 10) [1;2;5] Return_none
let test2 () = flexible_find ~f:(fun x -> x > 10) [1;2;5] (Default_to 10)  
let test3 () = flexible_find ~f:(fun x -> x > 10) [1;2;20] Raise


(* catch unknown *)
type stringable =
  Stringed : {value : 'a; to_string: 'a -> string} -> stringable

let stringables =
  (let s value to_string = Stringed {value; to_string} in
    [ s 100 Int.to_string
    ; s 12.3 Float.to_string
    ; s "foo" Fn.id]
  )

(* the type of underlying values can't escape the scope of stringable *)
(* let get_value (Stringed s) = s.value *)
(* how to get the internal data???? *)

(* combinator *)
module type Pipeline = sig
  type ('input, 'output) t
  
  val ( @> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val empty : ('a, 'a)t
end


module Example_pipeline (Pipeline : Pipeline) = struct
  open Pipeline
  let sum_file_sizes =
    (fun () -> Sys_unix.ls_dir ".")
    @> List.filter ~f:Sys_unix.is_file_exn
    @> List.map ~f:(fun file_name -> (Core_unix.lstat file_name).st_size)
    @> List.sum (module Int) ~f:Int64.to_int_exn
    @> empty

  let res =
    Int.of_string
    @> (+) 1
    @> empty

  end

module Basic_pipeline : sig
  include Pipeline
  val exec : ('a, 'b) t -> 'a -> 'b
end = struct
  type ('input, 'output) t = 'input -> 'output
  let empty = Fn.id
  
  let ( @> ) f t input =  t (f input)

  let exec t input = t input
end


type (_, _) pipeline =
  | Step : ('a -> 'b) * ('b, 'c) pipeline -> ('a, 'c) pipeline
  | Empty : ('a, 'a) pipeline
  
let ( @> ) f pipeline = Step (f, pipeline)
let empty = Empty

let rec exec : type a b. (a, b) pipeline -> a -> b =
  fun pipeline input ->
    match pipeline with
    | Step (f, tail) -> exec tail (f input)
    | Empty -> input
    
open Core
let exec_with_profile pipeline input = 
  let rec loops 
    : type a b. (a, b) pipeline -> a -> Time_ns.Span.t list 
        -> b * Time_ns.Span.t list
  = fun pipeline input span ->
    match pipeline with
    | Empty -> (input, span)
    | Step (f, tail) -> 
      let start = Time_ns.now () in
      let output = f input in
      let elapsed = Time_ns.diff (Time_ns.now ()) start in
      loops tail output (elapsed :: span)
  in loops pipeline input [] 



(* narrow the possibility *)
type incomplete = Incomplete
type complete = Compelete  

type (_, _) coption =
  | Absent : (_, incomplete) coption
  | Present : 'a -> ('a, _) coption

let get ~default o =
  match o with
  | Present x -> x
  | Absent -> default

let get' (o : (_, complete) coption) =
  match o with
  | Present x -> x
  
let get'' (Present x : (_, complete) coption) = x

type 'c logon_request = {
  user_name : string ;
  user_id : (int, 'c) coption ;
  permission : (bool, 'c) coption ;
}

let set_user_id request x = {request with user_id = Present x}

let set_permissions request x = {request with permission = Present x}

let check_completeness request : complete logon_request option =
  match request.user_id, request.permission with
  | Absent, _ | _, Absent -> None
  | (Present _ as user_id) , (Present _ as permission) ->
    Some {request with user_id; permission}

let authorized (request : complete logon_request) =
  let {user_id = Present user_id; permission = Present permission; _} = request in
    check_completeness user_id 