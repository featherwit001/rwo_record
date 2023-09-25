open Base
module type X_int = sig val x : int end
module Three = struct let x = 3 end
module Four = struct let x = 4 end

let three = (module Three: X_int)
let numbers = [three; (module Four)]

module New_three = (val three : X_int)
let x = New_three.x


let to_int m =
  let module M = (val m : X_int) in
  M.x

let plus m1 m2 =
  (module struct 
    let x = to_int m1 + to_int m2 
  end : X_int)

let to_int' (module M : X_int) = M.x

let six = plus three three

let twelve = to_int (List.fold ~init:six ~f:plus [three; three])


module type Bumpable = sig
  type t
  val bump : t -> t
end

module Int_bumper = struct
  type t = int
  let bump n = n + 1
end

module Float_bumper = struct
  type t = float
  let bump n = n +. 1.
end

let int_bumper = (module Int_bumper : Bumpable with type t = int)
let float_bumper = (module Float_bumper : Bumpable with type t = float)
let res3 = 
  let (module Bumper) = int_bumper in
  Bumper.bump 3

let bump_list 
  (type a) 
  (module Bumper : Bumpable with type t = a) 
  (lst : a list)
  =
  List.map ~f:(Bumper.bump) lst  

let bump_list 
  : type a. 
    (module Bumpable with type t = a) -> a list -> a list
  = fun (module Bumper) lst ->
  List.map ~f:(Bumper.bump) lst  


module type Comparable = sig
  type t
  val compare : t -> t -> int
end

(* error local abstract type is a concrete type in the function
   but it will be refered into polymorphic type outside
   so a is uncompatible with int --- another concreate type*)
(* let double (type a) (x : a) = x + x  *)

let create_comparable (type a) compare =
  (module struct
   type t = a
   let compare = compare  
  end : Comparable with type t = a)