open Core
open Async

let rec copy_blocks buffer r w = 
  match%bind Reader.read r buffer with
  | `Eof -> return ()
  | `Ok bytes_read ->
      Writer.write w (Bytes.to_string buffer) ~len:bytes_read;
      let%bind () = Writer.flushed w in
      copy_blocks buffer r w
  
let run () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port 8765)
      (* automatically shut down the connection *)
      (fun _addr r w ->
        let buffer = Bytes.create (16 * 1024) in
        copy_blocks buffer r w)
  in
  ignore
    (host_and_port
      : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

(* let () =
  run ();
  never_returns (Scheduler.go ()) *)


(* Command version *)

let run ~uppercase ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun _addr r w ->
        (* set up a process to take data from [reader] 
           and transfrom the data by [f]
           and transfer data to [writer].
           The connectiion will be shutdown when the read has benn closed and
           the last element is transferred to the write *)
        Pipe.transfer
          (Reader.pipe r)
          (Writer.pipe w)
          ~f:(if uppercase then String.uppercase else Fn.id))
  in
  ignore
    (host_and_port
      : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  (* a never determined deferred server never shut down*)
  Deferred.never ()

let () =
  Command.async
    ~summary:"Start an echo server"
    (let%map_open.Command uppercase =
       flag
         "-uppercase"
         no_arg
         ~doc:" Convert to uppercase before echoing back"
     and port =
       flag
         "-port"
         (optional_with_default 8765 int)
         ~doc:" Port to listen on (default 8765)"
     in
     fun () -> run ~uppercase ~port)
  |> Command_unix.run




