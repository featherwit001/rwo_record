open Core

let sexplist = Sexp.List [
    Sexp.Atom "this";
    Sexp.List [ Sexp.Atom "is"; Sexp.Atom "an"];
    Sexp.List [ Sexp.Atom "s"; Sexp.Atom "expression"]
]

let sexp_string = Sexp.to_string sexplist
let seplist = Sexp.of_string sexp_string

(* Int string and exn *)
let int3 = Int.sexp_of_t 3

let str = String.sexp_of_t "Hello"

let invalid () = Exn.sexp_of_t (invalid_arg "foo")

let listsexp = List.sexp_of_t Int.sexp_of_t [1; 2; 3]
let list = List.t_of_sexp Int.t_of_sexp (Sexp.of_string "(1 2 3)")
(* fail when sexp don't match the structure of OCaml type in question *)


module BrandNew = struct 
  type t = {foo : int ; bar : float}
  let sexp_of_t t = 
    let a x = Sexp.Atom x and l x = Sexp.List x in
    l [ l [a "foo"; Int.sexp_of_t t.foo];
        l [a "bar"; Float.sexp_of_t t.bar] ]

  (* # sexp_of_t {foo = 3; bar = -5.5};;
      - : Sexp.t = ((foo 3) (bar -5.5)) *)
end

module BrandNew' = struct 
  type t = {foo : int ; bar : float}
  [@@deriving sexp]

end


(* raise exception with location *)
exception Exn_with_sexp of string list [@@deriving sexp]

let r () = raise (Exn_with_sexp ["1"; "2"; "3"])

(* inline declarations *)
let tup_to_sexp = [%sexp_of : int * string ]
let tup = [%sexp_of : int * string] (3, "1")


type no_converter = int * int

(* error because no_converter don't have a sexp converter *)
(* type t = { a: no_converter; b: string} [@@deriving sexp] *)

type t = { a: (no_converter [@sexp.opaque]); b: string} [@@deriving sexp]

type compatible_versions =
  | Specific of string list [@sexp.list]
  | All
[@@deriving sexp]

type toption =
  { a: int option [@sexp.option];
    b: string;
  } [@@deriving sexp]


type http_server_config = {
  web_root: string;
  port: int [@default 80];
  addr: string [@default "localhost"];
} [@@deriving sexp]  


let cfg =
  "((web_root /var/www/html))"
  |> Sexp.of_string
  |> http_server_config_of_sexp


type http_server_config' = {
  web_root: string;
  port: int [@default 80] [@sexp_drop_default.equal];
  addr: string [@default "localhost"] [@sexp_drop_default.equal];
} [@@deriving sexp]

let cfg =
  "((web_root /var/www/html))"
  |> Sexp.of_string
  |> http_server_config'_of_sexp

