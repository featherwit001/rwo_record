type node = {
  id : int;
  col : int;
  row : int;
  mutable up : node ref option;
  mutable down : node ref option;
  mutable left : node ref option;
  mutable right : node ref option;
}
type answer_node = {
  ans : int array;
  dep : int;
  mutable next : answer_node ref option;
}
type answers = {
  mutable length : int;
  mutable first : answer_node ref option;
}
type dancelist = {
  colen : int;
  rowlen : int;
  head : node;
  cols : node ref option array;
  col_sz : int array;
  rows : node ref option array;
  ans : int array;
  mutable dep : int;
  answers : answers;
}

(* main interface *)
val build : int -> int -> dancelist
val insert : int -> int -> dancelist -> unit
val dance : ?find_one:bool -> int -> dancelist -> bool

(** this function define how to resolve one answer which passed to resolve_dl *)
val print_rows : int array -> int -> unit
val resolve_dl : (int array -> int -> unit) -> dancelist -> unit


(* dancelist transform *)
val to_array : dancelist -> int array array
val to_id_array : dancelist -> int array array
val print_dl : dancelist -> unit
val to_array_by_row : dancelist -> int array array
val to_array_by_rcol : dancelist -> int array array
val to_array_by_lcol : dancelist -> int array array
val of_array : int array array -> dancelist




(* tests *)
val arr_easy : int array array
val arr : int array array
val arr' : int array array
val test1 : unit -> unit
val test_find_all : unit -> unit
