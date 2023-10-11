open Core

(* main function *)
let do_hash hash_length filename = 
  Md5.digest_file_blocking filename 
  |> Md5.to_hex
  |> (fun s -> String.prefix s hash_length) 
  |> print_endline

(* parameter parser *)
let filename_param = 
  let open Command.Param in
  anon ("[filename]" %: string)

(* multiple arguments *)
let commandv1 = 
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.(map 
      (both
        (anon ("[hash_length]" %: int))
        (anon ("[filename]" %: string)))
      ~f:(fun (hash_length, filename) () -> 
          do_hash hash_length filename)))

(* let extension 1 *)
let commandv2 = 
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let open Command.Let_syntax in
    let open Command.Param in
    let%map hash_length = anon ("hash_length" %: int)
    and filename = anon ("filename" %: string) in
    fun () -> do_hash hash_length filename)

(* let extension 2 *)
let commandv2' = 
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (* using Let-syntax extention form the following module Command  *)
    (* but how to using extension?? *)
    (let%map_open.Command 
      hash_length = anon ("hash_length" %: int)
      and 
      (* pre-defined argument type *)
      filename = anon ("filename" %: Filename.arg_type)
    (* and path = anon ("path" %: string)  *)
    (* .... *)
    in
    fun () -> do_hash hash_length filename)

(* ----------------------------------------------------------------- *)

(* define Argument Type *)
let regular_file =
  Command.Arg_type.create (fun filename -> 
    match Sys.is_file filename with
    | `Yes -> filename
    | `No -> failwith "Not a regular file"
    | `Unknown -> 
      failwith "Could not determine if this was a regular file")
       
let do_hashv3 filename = 
Md5.digest_file_blocking filename |> Md5.to_hex |> print_endline

(* use custom type of Arg *)
let commandv3 = 
  Command.basic
  ~summary:"Generate an MD5 hash of the input data"
  ~readme:(fun () -> "More detailed information")
  (let%map_open.Command filename = 
    anon ("filename" %: regular_file)
    in
    fun () -> do_hashv3 filename)


(* option Arg *) 
let get_contents = function
  | None | Some "-" -> In_channel.input_all In_channel.stdin
  | Some filename -> In_channel.read_all filename

let do_hashv4 filename =
  get_contents filename
  |> Md5.digest_string
  |> Md5.to_hex
  |> print_endline

let commandv4 =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command filename =
       anon (maybe ("filename" %: Filename.arg_type))
     in
     fun () -> do_hashv4 filename)

(* option with default *)
let get_contents' = function
| "-" -> In_channel.input_all In_channel.stdin
| filename -> In_channel.read_all filename

let do_hashv5 filename =
  get_contents' filename
  |> Md5.digest_string
  |> Md5.to_hex
  |> print_endline

let commandv5 =
Command.basic
  ~summary:"Generate an MD5 hash of the input data"
  ~readme:(fun () -> "More detailed information")
  (let%map_open.Command filename =
    anon (maybe_with_default  "-" ("filename" %: Filename.arg_type))
  in
  fun () -> do_hashv5 filename)

(* sequences  *)

let do_hashv6 filename =
  get_contents' filename
  |> Md5.digest_string
  |> Md5.to_hex
  |> fun md5 -> printf "MD5 (%s) = %s\n" filename md5

let commandv6 =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command files =
       anon (sequence ("filename" %: Filename.arg_type))
     in
     fun () ->
       match files with
       | [] -> do_hashv6 "-"
       | _ -> List.iter files ~f:do_hashv6)


(* labeled flags *)
let checksum_from_string buf =
  Md5.digest_string buf |> Md5.to_hex |> print_endline

let checksum_from_file filename =
  let contents =
    match filename with
    | "-" -> In_channel.input_all In_channel.stdin
    | filename -> In_channel.read_all filename
  in
  Md5.digest_string contents |> Md5.to_hex |> print_endline
  
let commandv7 = 
  Command.basic
  ~summary:"Generate an MD5 hash of the input data"
  (let%map_open.Command 
    use_string = flag "-s" (optional string) ~doc:"string Checksum the given string"
    and
    trial = flag "-t" no_arg ~doc:"run a built-in time trial"
    and 
    filename = anon (maybe_with_default "-" ("filename" %: Filename.arg_type))  
  in
    fun () -> 
      if trial 
      then printf "Running time trial\n"
      else (
        match use_string with
        | Some buf -> checksum_from_string buf
        | None -> checksum_from_file filename)
  )


let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" commandv7



