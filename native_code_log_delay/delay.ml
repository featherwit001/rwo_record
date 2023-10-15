open Core
open Async

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


let noalloc_busy_loop () =
  for i = 0 to 100_000_000 do () done

(* without output??? *)
let test4 () = log_delays (fun () -> In_thread.run noalloc_busy_loop)