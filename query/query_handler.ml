open Query_handler_core


let unique_instance = build_instance (module Unique) 0
let list_dir_instance = build_instance (module List_dir) "/var"
let () =
  cli (build_dispatch_table [unique_instance; list_dir_instance])
