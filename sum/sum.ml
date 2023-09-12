open Base
open Stdio

let rec read_and_acc acc = 
  let line = In_channel.input_line In_channel.stdin in 
  match line with
  | None -> acc
  | Some x -> read_and_acc (acc +. Float.of_string x)

let () =
  (* need to type EOF (Ctrl-D) to end the input *)
  printf "Total: %F\n" (read_and_acc 0.)