(* functor basic usage *)
module type T = sig
  type t
  val x : t
end

module Pair1 (M : T) = struct
  let p = (M.x, 1) 
end

(* or *)
module type P1 = functor (M : T) -> sig val p : M.t * int end

module Pair1' : P1 = functor (M : T) -> struct
  let p = (M.x, 1)
end
(* functor output signature i.e. module type *)





(* functor one argument *)

(* input signature : module type*)
module type FTYPE = functor (M: sig val x : int end) -> sig val y : int end
module F (M : sig val x : int end) = struct let y = M.x end
module F'  = (functor(M : sig val x : int end) -> struct let y = M.x end: FTYPE)
module X = struct let x = 0 end
module Z = struct let x = 0 ;; let z = 0 end

(* have to wrap parenthese *)
module FX = F (X)
module FZ = F (Z)


(* two argument functor *)
module T0 : T = struct
  type t = int
  let x = 0
end

module TA : T = struct
  type t = char
  let x = 'a'
end

module P0 = Pair1 (struct type t = int let x = 0 end)
module PA = Pair1 (struct type t = char let x = 'a' end)


module type F2TYPE = functor (M1 : T) -> functor (M2 : sig val x : int end) -> 
  sig val z : M1.t val y : int end

module F2 (M1 : T )(M2 : sig val x : int end) = struct let z = M1.x let y = M2.x end

module X' = struct let x = 0 end
module Z' = struct let x = 0 ;; let z = 0 end

(* have to wrap parenthese *)
module F2X = F2 (T0) (X')
module F2Z = F2 (TA) (Z')

(* functor type : functor ( M :<module type1> )-> <module type2> *)

(* F2 type *)


(* encapsulation *)
module F2' : F2TYPE = F2

(* or *)
module type F2TYPE' = functor (M1: T) (M2: sig val x : int end) -> 
  sig val z : M1.t val y: int end
module F2'' : F2TYPE' = functor (M1 : T) -> functor (M2 : sig val x : int end) -> struct 
  let z = M1.x let y = M2.x end






module MyModule = struct
  type t = int
  let add a b = a + b
end
module type MyModuleType = sig val add : int -> int -> int end

(* module as function paramenter *)
let use_module (module M : MyModuleType) =
  M.add 3 4
let result = use_module (module MyModule)




(* module as value *)
let my_module = (module MyModule : MyModuleType)
let res =
    let module M = (val my_module : MyModuleType) in 
    M.add 3 4
(* or *)
let res =
  let (module M) = my_module in 
  M.add 3 4


(* module as a member of one data structure*)
type module_container = { mod_value : (module MyModuleType) }

let my_container = { mod_value = (module MyModule) }
let result = 
  let (module M) = my_container.mod_value in 
    M.add 3 4
(* or *)
let result = 
  let module M = (val my_container.mod_value) in 
  M.add 3 4
 

(* final *)
(* wrap and unwrap *)
let res = 
  let module M = (val (module MyModule : MyModuleType)  (*: MyModuleType*) )  in 
  M.add 3 4


(* functor and first-class module *)
(* module type *)
module type SayHello = sig
  val say_hello : string -> string
end

(* module implement *)
module HelloSb = struct
  let say_hello sb = "hello, " ^ sb
end

(* functors *)
module PrintHello (S: SayHello) = struct
  let print_greeting s = print_endline (S.say_hello s)
end  

(* first module must be tagged with module type and wrapped with (module xxx : tttt)
  eg.   
   (module <module implement> : <module type>) *)
let mymodules = [(module HelloSb : SayHello)]

let pp lst =
  let hh m =  
    (* unwrap module form module  *)
    let module H = (val m : SayHello) in 
    let module P = PrintHello (H) in
    P.print_greeting "fea01"
  in
  lst |> List.map hh 

(* let _ = pp mymodules *)  