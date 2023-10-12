
(* examples *)
val _sudoku0 : int array array
val sudoku1 : int array array
val sudoku1' : int array array
val sudoku2 : int array array
val sudoku3 : int array array
val sudoku4 : int array array
val sudoku5 : int array array
val sudoku6 : int array array
val sudoku7 : int array array
val sudoku8 : int array array
val sudoku9 : int array array

(* internal implement *)
val constrain : int -> int -> int -> Dancelist.dancelist -> unit
val zero_constrain : int -> int -> Dancelist.dancelist -> unit
val not_zero_constrain : int -> int -> int -> Dancelist.dancelist -> unit
val sudoku_to_dl : int array array -> Dancelist.dancelist
val sudoku_to_string : int array array -> string
val answer_to_sudoku : int -> int array -> int array array
val resolve_one_sudoku : int array -> int -> unit

(* test *)
val test2 : unit -> unit
val test3 : unit -> unit


(** main interface *)
val solve_sudoku : int array array -> unit
val solve_sudoku_all : int array array -> unit
val time : (unit -> 'a) -> 'a
