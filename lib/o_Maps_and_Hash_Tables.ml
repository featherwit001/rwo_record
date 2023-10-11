open Base

let digit_alist =
  [ 0, "zero"; 1, "one"; 2, "two"  ; 3, "three"; 4, "four"
  ; 5, "five"; 6, "six"; 7, "seven"; 8, "eight"; 9, "nine" ]

let res = List.Assoc.find ~equal:Int.equal digit_alist 6
let res = List.Assoc.find ~equal:Int.equal digit_alist 22
let res = List.Assoc.add ~equal:Int.equal digit_alist 0 "zilch"



module Counter = struct
  (* String.comparator_witness  is needed *)
  type t = (string, int, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let to_list t = Map.to_alist t

  let touch t s =
    let count =
      match Map.find t s with
      | None -> 0
      | Some x -> x
    in
    Map.set t ~key:s ~data:(count + 1)
  end

(* satify the interface of Base.Comparator.S *)
module Book = struct
  module T = struct
    type t = { title : string; isbn : string }

    let compare t1 t2 = 
      let cmp_title = String.compare t1.title t2.title in
      if cmp_title <> 0 then cmp_title 
      else String.compare t1.isbn t2.isbn

    let sexp_of_t t : Sexp.t = 
      List [Atom t.title; Atom t.isbn]
  end

  include T
  include Comparator.Make (T)

end

let some_programming_books =
  Set.of_list (module Book)
    [ { title = "Real World OCaml"
      ; isbn = "978-1449323912" }
    ; { title = "Structure and Interpretation of Computer Programs"
      ; isbn = "978-0262510875" }
    ; { title = "The C Programming Language"
      ; isbn = "978-0131101630" } ]


let left = Map.of_alist_exn (module String) [("foo",1); ("bar",3); ("snoo", 0)]
let right = Map.of_alist_exn (module String) [("foo", 0); ("snoo", 0)]
(* Map.symmetric_diff require tow map have the same 'cmp *)
let res = Map.symmetric_diff ~data_equal:Int.equal left right |> Sequence.to_list



(* Polymorphic comparator *)
let res = Map.Poly.of_alist_exn digit_alist


(* let err = Map.symmetric_diff 
          (Map.Poly.singleton 3 "three")
          (Map.singleton (module Int) 3 "four") *)



(* syntax-extentions *)
(* open Ppx_jane *)
module Book = struct
  module T = struct
    type t = { title : string; isbn : string }
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)

end

(* error for s-expression can not parse comparator witness *)
(* type string_int_map = (string, int, String.comparator_witness) Map.t [@@deriving sexp] *)

type string_int_map = int Map.M(String).t [@@deriving sexp]
(* the type is the same as *)
let m = Map.singleton (module String) "one" 1
let m = (m : int Map.M(String).t)


(* trees: expose the internal data representation *)
let ord_map = left
let ord_tree= Map.Using_comparator.to_tree ord_map
let res = Map.Using_comparator.Tree.find ~comparator:String.comparator ord_tree "snoo"

(* phantom type is there to enforce using the same comparator *)
(* let res = Map.Using_comparator.Tree.find 
    ~comparator:Int.comparator ord_tree "snoo" *)

let table = Hashtbl.create (module String)

let () = Hashtbl.set table ~key:"three" ~data:3
let res = Hashtbl.find table "three"

module Book' = struct
  type t = { title: string; isbn: string }
  [@@deriving compare, sexp_of, hash]
end

(* Poly hash using Breadth-first traversal whose bound is ten.
   so every data structure whose length larger than 10 and their prefix element is the same
   their hash value is the same *)
let table = Hashtbl.Poly.create ()

let res1 = Hashtbl.Poly.hashable.hash (List.range 0 9)
let res2 = Hashtbl.Poly.hashable.hash (List.range 0 10)
let res3 = Hashtbl.Poly.hashable.hash (List.range 0 11)
let res4 = Hashtbl.Poly.hashable.hash (List.range 0 100) 
(* res2 = res3 = res4 , so avoid using Poly*)



let () = Hashtbl.set table ~key:("foo", 3, [1; 2; 3]) ~data:"random data!"
let res = Hashtbl.find table ("foo", 3, [1; 2; 3])


let res = [%hash : int list] (List.range 0 9)

