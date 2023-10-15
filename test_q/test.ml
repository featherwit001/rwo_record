open Base
open Stdio 
let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let%test _ = fact 5 = 120


(* no outputs if it passes *)
let%test "rev" = 
  List.equal Int.equal (List.rev [3; 2; 1]) [1; 2; 3]


let%test_unit "rev" = 
  [%test_eq : int list ] (List.rev [3; 2; 1]) [1; 2; 3]


let%expect_test "trivial" = 
  print_endline "Hello World!";
  [%expect {| Hello World! |}]


let%expect_test _ = 
  print_s [%sexp (List.rev [3; 2; 1] : int list)];
  [%expect {| (1 2 3) |}]

let%test "rev" = 
  (List.equal Int.equal (List.rev [1; 2; 3]) [3; 2; 1])  


let get_href_hosts soup =
  Soup.select "a[href]" soup
  |> Soup.to_list
  |> List.map ~f:(Soup.R.attribute "href")
  |> List.filter_map ~f:(fun uri -> Uri.host (Uri.of_string uri))
  |> Set.of_list (module String)

let%expect_test _ =
  let example_html =
    {|
    <html>
      Some random <b>text</b> with a
      <a href="http://ocaml.org/base">link</a>.
      And here's another
      <a href="http://github.com/ocaml/dune">link</a>.
      And here is <a>link</a> with no href.
    </html>|}
  in
  let soup = Soup.parse example_html in
  let hrefs = get_href_hosts soup in
  print_s [%sexp (hrefs : Set.M(String).t)];
  [%expect {| (github.com ocaml.org) |}]


(* QuickCheck *)
let%test_unit "negation flips the sign" = 
  for _ = 0 to 100_000 do
    let x = Random.int_incl Int.min_value Int.max_value in
    [%test_eq : Sign.t] 
    (Int.sign (Int.neg x))
    (Sign.flip (Int.sign x))
  done

open Core 

(* the minest int value's abs is smaller than the biggest int value 
   neg the Int.min_value will get itself *)

let%test_unit "negation flips the sign" =
  Quickcheck.test
    ~sexp_of:[%sexp_of:int]
    (Int.gen_incl (Int.min_value + 1) Int.max_value)
    ~f:(fun x->
      [%test_eq : Sign.t] 
      (Int.sign (Int.neg x))
      (Sign.flip (Int.sign x)))



let gen_int_list_pair =
  let int_list_gen =
    List.gen_non_empty (Int.gen_incl Int.min_value Int.max_value)
  in
  Quickcheck.Generator.both int_list_gen int_list_gen

let%test_unit "List.rev_append is List.append of List.rev" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int list * int list]
    gen_int_list_pair
    ~f:(fun (l1, l2) ->
      [%test_eq: int list]
        (List.rev_append l1 l2)
        (List.append (List.rev l1) l2))

let%test_unit "List.rev_append is List.append of List.rev" =
Quickcheck.test
  ~sexp_of:[%sexp_of: int list * int list]
  [%quickcheck.generator: int list * int list]
  ~f:(fun (l1, l2) ->
    [%test_eq: int list]
      (List.rev_append l1 l2)
      (List.append (List.rev l1) l2))

type shape =
  | Circle of { radius: float } [@quickcheck.weight 0.5]
  | Rect of { height: float; width: float }
  | Poly of (float * float) list
[@@deriving quickcheck]


let gen_shape =
  let open Quickcheck.Generator.Let_syntax in
  let module G = Base_quickcheck.Generator in
  let circle =
    let%map radius = G.float_positive_or_zero in
    Circle { radius }
  in
  let rect =
    let%bind height = G.float_positive_or_zero in
    let%map width = G.float_inclusive height Float.infinity in
    Rect { height; width }
  in
  let poly =
    let%map points =
      List.gen_non_empty
        (G.both G.float_positive_or_zero G.float_positive_or_zero)
    in
    Poly points
  in
  G.union [ circle; rect; poly ]
