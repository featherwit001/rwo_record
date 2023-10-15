open Core

let prompt_for_string name of_string =
  printf "enter %s: %!" name;
  match In_channel.input_line In_channel.stdin with
    | None -> failwith "no value entered. aborting."
    | Some line -> of_string line


let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    (let%map_open.Command base = anon ("base" %: date)
     and days = anon (maybe ("days" %: int)) in
      (* prompt *)
     let days = match days with
      | Some x -> x
      | None -> prompt_for_string "days" Int.of_string in
     fun () ->
       Date.add_days base days |> Date.to_string |> print_endline)

let diff = 
  Command.basic
    ~summary:"Show days between [date1] and [date2]"
    (let%map_open.Command date1 = anon ("date1" %: date)
      and date2 = anon ("date2" %: date) in
      fun () -> Date.diff date1 date2 |> printf "%d days\n")

(* prompt *)
let anon_prompt name of_string = 
  (* Arg_type.create need a function : string -> 'a *)
  let arg = Command.Arg_type.create of_string in
  let%map_open.Command value = anon (maybe (name %: arg)) in
  match value with
  | Some v -> v
  | None -> prompt_for_string name of_string

let add' =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    (let%map_open.Command base = anon ("base" %: date)
      and days = anon_prompt "days" Int.of_string in
      fun () ->
        Date.add_days base days |> Date.to_string |> print_endline)

let command = 
  Command.group
    ~summary:"Manipulate dates"
    ["add", add';
     "diff", diff]

let () = Command_unix.run command
