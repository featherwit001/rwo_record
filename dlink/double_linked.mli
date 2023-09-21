type 'a elt 
(* = 
{
  value : 'a;
  mutable prev : 'a elt option;
  mutable next : 'a elt option;
} *)
type 'a dlink 
(* = { mutable head : 'a elt option; mutable len : int; } *)
val create : unit -> 'a dlink
val is_empty : 'a dlink -> bool
val first : 'a dlink -> 'a elt option
val next : 'a elt -> 'a elt option
val prev : 'a elt -> 'a elt option
val value : 'a elt -> 'a

val iter : 'a dlink -> f:('a -> unit) -> unit
val find_el : 'a dlink -> f:('a -> bool) -> 'a elt option

val insert_first : 'a dlink -> 'a -> 'a elt
val insert_after : 'a dlink -> 'b elt -> 'b -> 'b elt
val remove : 'a dlink -> 'a -> unit

val of_list : 'a list -> 'a dlink
val to_list : 'a dlink -> 'a list
