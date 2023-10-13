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


(* 伪需求 *)
module type Delayer_intf = sig
  type t
  val create : Time.Span.t -> t
  val schedule : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
end

module Delayer : Delayer_intf = struct
  type t = { delay: Time.Span.t;
             jobs: (unit -> unit) Queue.t;
           }

  let create delay =
    { delay; jobs = Queue.create () }

  let schedule t thunk =
    let ivar = Ivar.create () in
    Queue.enqueue t.jobs (fun () ->
      (* upon: 'a t -> ('a -> 'b) -> 'b t *)
      upon (thunk ()) (fun x -> Ivar.fill ivar x));
    upon (after t.delay) (fun () ->
      let job = Queue.dequeue_exn t.jobs in
      job ());
    Ivar.read ivar
end
