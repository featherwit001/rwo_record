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


(* 
>>> (loader known_services)        
(ls loader unique)
>>>  (loader active_services)      
(loader)
>>> (ls .)
("Could not find matching handler" ls)
>>> (loader (load ls /var))        
()
>>> (ls .)
(metrics lock opt local tmp snap backups www spool crash cache log lib run
 mail)
>>>  (loader (unload loader))
It's unwise to unload yourself


>>> (loader (load unique 0))       
()
>>> (unique ())
0
>>> (unique ())
1
>>> (loader (unload unique))
()
>>> (unique ())
("Could not find matching handler" unique)

>>> quit
*)
