open Base
open Core
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


module type Bumpable' = sig
  type t
  type k
  val bump : t -> t
end

module Int_float_bumper = struct
  type t = int
  type k = float
  let bump n = n + 1
end

let int_float_bumper = 
  (module Int_float_bumper 
    : Bumpable' with type t = int and type k = float)

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


module type Query_handler = sig

  (** Configuration for a query handler *)
  type config

  val sexp_of_config : config -> Sexp.t
  val config_of_sexp : Sexp.t -> config

  (** The name of the query-handling service *)
  val name : string

  (** The state of the query handler *)
  type t

  (** Creates a new query handler from a config *)
  val create : config -> t

  (** Evaluate a given query, where both input and output are
      s-expressions *)
  val eval : t -> Sexp.t -> Sexp.t Or_error.t
end


module Unique = struct
  type config = int [@@deriving sexp]
  type t = {mutable next_id : int}

  let name = "unique"
  let create start_at = {next_id = start_at}

  let eval t sexp =
    match Or_error.try_with (fun () -> unit_of_sexp sexp) with
    | Error _ as err -> err
    | Ok () ->
      let response = Ok (Int.sexp_of_t t.next_id) in
      t.next_id <- t.next_id + 1;
      response
end

let unique = Unique.create 0
let res = Unique.eval unique (Sexp.List [])
let res = Unique.eval unique (Sexp.List [])


module List_dir = struct
  type config = string [@@deriving sexp]
  type t = { cwd: string }

  (** [is_abs p] Returns true if [p] is an absolute path  *)
  let is_abs p =
    String.length p > 0 && Char.(=) p.[0] '/'

  let name = "ls"
  let create cwd = { cwd }

  let eval t sexp =
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
    | Error _ as err -> err
    | Ok dir ->
      let dir =
        if is_abs dir then dir
        else Core.Filename.concat t.cwd dir
      in
      (* or Sys_unix *)
      Ok (Array.sexp_of_t String.sexp_of_t (Sys_unix.readdir dir))
end


module type Query_handler_instance = sig
  module Query_handler : Query_handler
  val this : Query_handler.t
end

let build_instance 
  (type a)
  (module Q : Query_handler with type config = a)
  config
  =
  (module struct
    module Query_handler = Q
    let this = Q.create config  
  end : Query_handler_instance)

let unique_instance = build_instance (module Unique) 0
let list_dir_instance = build_instance (module List_dir) "/var"


let build_dispatch_table handlers =
  let table = Hashtbl.create (module String) in
  List.iter handlers
    ~f:(fun ((module I : Query_handler_instance) as instance )->
        Hashtbl.set table ~key:I.Query_handler.name ~data:instance);
  table

(* (query-name query) *)
let dispatch dispatch_table name_and_query =
  match name_and_query with
  | Sexp.List [Sexp.Atom name; query] ->
    begin match Hashtbl.find dispatch_table name with
    | None -> 
        Or_error.error "Could not find matching handler" name String.sexp_of_t
    | Some (module I : Query_handler_instance) -> 
        I.Query_handler.eval I.this query
    end
  | _ -> Or_error.error_string "malformed query"


open Stdio
let rec cli dispatch_table =
  printf ">>> %!";
  let result =
    match In_channel.(input_line stdin) with
    | None -> `Stop
    | Some line -> 
      match Or_error.try_with (fun () ->
        Core.Sexp.of_string line)
      with
      | Error e -> `Continue (Error.to_string_hum e) 
      | Ok (Sexp.Atom "quit") -> `Stop
      | Ok query ->
        begin match dispatch dispatch_table query with
        | Error e -> `Continue (Error.to_string_hum e)
        | Ok s -> `Continue (Sexp.to_string_hum s)
      end;
    in
    match result with
    | `Stop -> ()
    | `Continue msg -> 
      printf "%s\n%!" msg;
      cli dispatch_table

(* let () = cli (build_dispatch_table [unique_instance; list_dir_instance]) *)
