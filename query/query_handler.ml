open Query_handler_core

let () =
  cli (build_dispatch_table [unique_instance; list_dir_instance])
