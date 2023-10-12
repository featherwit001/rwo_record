open Core
open Async

let uppercase_file filename =
  Deferred.bind (Reader.file_contents filename)
    ~f: (fun text ->
      Writer.save filename ~contents:(String.uppercase text))

let uppercase_file filename =
  (Reader.file_contents filename)
    >>= fun text ->
      Writer.save filename ~contents:(String.uppercase text)

(* Defered.bind >>= *)
let count_lines filename =
    Reader.file_contents filename      
    >>= fun text ->
      return (List.length (String.split text ~on:'\n'))

(* Defered.map >>| *)
let count_lines' filename =
  Reader.file_contents filename
  >>| fun text ->
    List.length (String.split text ~on:'\n')


(* let syntax *)
let count_lines'' filename =
  let%bind text = Reader.file_contents filename in
  return (List.length (String.split text ~on:'\n'))

let count_lines''' filename =
  let%map text = Reader.file_contents filename in
  List.length (String.split text ~on:'\n')

let ivar = Ivar.create ()
let def = Ivar.read ivar
let res = Deferred.peek def

let () = Ivar.fill ivar "Hello"
let res = Deferred.peek def 