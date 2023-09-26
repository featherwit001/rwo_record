open Core
open Query_handler_core

let () =
  let loader = Loader.create [(module Unique); (module List_dir); (module Loader)] in
  let loader_instance =
    (module struct
       module Query_handler = Loader
       let this = loader
     end : Query_handler_instance)
  in
  Hashtbl.set loader.active
    ~key:Loader.name ~data:loader_instance;
  cli loader.active