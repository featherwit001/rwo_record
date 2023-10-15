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

(* Defered.bind >>=  'a t -> ('a -> 'b t) -> 'b t *)
let count_lines filename =
    Reader.file_contents filename      
    >>= fun text ->
      return (List.length (String.split text ~on:'\n'))

(* Defered.map >>| 'a t -> ('a -> 'b) -> 'b t *)
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

(* async !! odd call is unit and 
   even call will raise exception
   this examole only works in utop line *)
let maybe_raise =
  let should_fail = ref false in
  fun () ->
    let will_fail = !should_fail in
    should_fail := not will_fail;
    let%map () = after (Time.Span.of_sec 0.5) in
    if will_fail then raise Exit else ()

(* only capture synchrnous exceptions *)
let handle_error () =
  try
    let%map () = maybe_raise () in
    "success"
  with _ -> return "failure"

(* Async.try_with  *)
let handle_error () =
  match%map try_with (fun () -> maybe_raise ()) with
  | Ok ()   -> "success"
  | Error _ -> "failure"

(* monitor form Erlang *)
let blow_up () =
  let monitor = Monitor.create ~name:"blow up monitor" () in
  within' ~monitor maybe_raise

let swallow_error () =
  let monitor = Monitor.create () in
  Stream.iter (Monitor.detach_and_get_error_stream monitor)
    ~f:(fun _exn -> printf "an error happened\n");
  within' ~monitor (fun () ->
    let%bind () = after (Time.Span.of_sec 0.25) in
    failwith "Kaboom!")


let testswallow () = Deferred.any [ after (Time.Span.of_sec 0.5); swallow_error () ]

exception Ignore_me
exception Another_exception
let swallow_some_errors exn_to_raise =
  let child_monitor  = Monitor.create  () in
  let parent_monitor = Monitor.current () in
  Stream.iter
  (* 提取monitor中的error信息 *)
    (Monitor.detach_and_get_error_stream child_monitor)
    (* function process specific exception and send other exns to parent *)
    ~f:(fun error ->
      match Monitor.extract_exn error with
      | Ignore_me -> printf "ignoring exn\n"
      | _ -> Monitor.send_exn parent_monitor error);
  within' ~monitor:child_monitor (fun () ->
    let%bind () = after (Time.Span.of_sec 0.25) in
    raise exn_to_raise)


let testswallowsome () = Deferred.any [ after (Time.Span.of_sec 0.5); 
                                      swallow_some_errors Ignore_me ]

                                     
let string_and_float = 
  Deferred.both
    (let%map () = after (sec 0.5) in "A")
    (let%map () = after (sec 0.25) in 32.33)

let deferred_any = 
  Deferred.any
  [ (let%map () = after (sec 0.5) in "half a second")
  ; (let%map () = after (sec 1.0) in "one second")
  ; (let%map () = after (sec 4.0) in "four seconds")
  ]

let def = In_thread.run (fun () -> List.range 1 10)

let log_delays thunk =
  let start = Time.now () in
  let print_time () =
    let diff = Time.diff (Time.now ()) start in
    printf "%s, " (Time.Span.to_string diff)
  in
  let d = thunk () in
  Clock.every (sec 0.1) ~stop:d print_time;
  let%bind () = d in
  printf "\nFinished at: ";
  print_time ();
  printf "\n";
  Writer.flushed (force Writer.stdout)

let test1 () = log_delays (fun () -> after (sec 0.5))

let busy_loop () =
  let x = ref None in
  for i = 1 to 100_000_000 do x := Some i done

let test2 () = log_delays (fun () -> return (busy_loop()))

let test3 () = log_delays (fun () -> In_thread.run (busy_loop))

let noalloc_busy_loop () =
  for i = 0 to 100_000_000 do () done

let test4 () = log_delays (fun () -> In_thread.run noalloc_busy_loop)

