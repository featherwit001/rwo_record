open Base
module type X_int = sig val x : int end

module Increment (M1 : X_int) (M2: X_int) : X_int = struct
  let x = M1.x + M2.x + 1
end 


module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type Interval_intf = sig
  type t
  type endpoint
  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
  val intersect : t -> t -> t
end


module Make_interval(Endpoint : Comparable) 
  : (Interval_intf with type endpoint := Endpoint.t)
  = struct
  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty
  (* [@@deriving sexp] *)
  (* lack the  Endpoint.t.sexp_of *)


  (** [create low high] creates a new interval from [low] to
      [high].  If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low,high)

  (** Returns true iff the interval is empty *)
  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  (** [contains t x] returns true iff [x] is contained in the
      interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l,h) ->
      Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  (** [intersect t1 t2] returns the intersection of the two input
      intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1,t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1,h1), Interval (l2,h2) ->
      create (max l1 l2) (min h1 h2)

end

module Int_interval =
    Make_interval(struct
      type t = int
      let compare = Int.compare
  end)

module Int_Inter = Make_interval (Int)
module String_Inter = Make_interval (String)

module Rev_int_interval =
    Make_interval(struct
      type t = int
      let compare x y = Int.compare y x
  end)

let sexplst1 = [Sexp.Atom "This"; Sexp.Atom "is"; 
  Sexp.List [Sexp.Atom "an"; Sexp.Atom "s-expression"]]

open Ppx_jane

type some_type = int * string list [@@deriving sexp]

let sexp1 = sexp_of_some_type (22, ["one"; "two"])
(* (33 (one two)) *)

let some_type1 = Core.Sexp.of_string "(44 (five six))" |> some_type_of_sexp 


module type Sexp = sig 
  type t
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end


(* example of Sexpable *)
module type Interval_intf_with_sexp = sig
  type t
  include Interval_intf with type t := t
  include Sexpable.S with type t := t
end 

(* type t could reuse the one defined in the first module which is included,
   but each module have its own type t 
   so define a new t and use this t to override the t in the modules included. *)

module Make_Interval (Endpoint : sig 
    type t
    include Comparable with type t := t
    include Sexpable.S with type t := t
  end)
  : (Interval_intf_with_sexp with type endpoint := Endpoint.t)
= struct
  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty
  [@@deriving sexp]
  (* the previous definition lacks the  Endpoint.t.sexp_of
     this one add the sexp-conversion information about Endpoint.t *)


  (** [create low high] creates a new interval from [low] to
      [high].  If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low,high)

  (** Returns true iff the interval is empty *)
  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  (** [contains t x] returns true iff [x] is contained in the
      interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l,h) ->
      Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  (** [intersect t1 t2] returns the intersection of the two input
      intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1,t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1,h1), Interval (l2,h2) ->
      create (max l1 l2) (min h1 h2)
end

(* the module Int need to [@@deriving sexp] *)
module Int_Interval = Make_Interval(Int)
let sexp2 = Int_Interval.sexp_of_t (Int_Interval.create 3 4)
let sexp3 = Int_Interval.sexp_of_t (Int_Interval.create 4 3)


(* auto extend the module with fold funciton *)

