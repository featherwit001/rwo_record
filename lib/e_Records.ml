(* open Re *)
open Base

type service_info = {
  service_name : string;
  port         : int ;
  protocol     : string;
}

let service_info_of_string line = 
  let matches = 
    let pat = "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)" in
      Re.exec (Re.Posix.compile_pat pat) line 
  in
  {
    service_name = Re.Group.get matches 1 ;
    port = Int.of_string (Re.Group.get matches 2);
    protocol = Re.Group.get matches 3 ;
  }

let ssh = service_info_of_string "ssh 22/udp"

type 'a with_line_num = { item : 'a ; line_num : int}


let parse_lines parse line = 
  let lines = String.split ~on:'\n' line in
  List.mapi lines ~f:(fun i e -> {
    item = parse e;
    line_num = i + 1;
  })

let real_services =   
  "rtmp              1/ddp     # Routing Table Maintenance Protocol
   tcpmux            1/udp     # TCP Port Service Multiplexer
   tcpmux            1/tcp     # TCP Port Service Multiplexer"

let lines = parse_lines service_info_of_string real_services

open Core

type client_info ={
  addr: Core_unix.Inet_addr.t;
  prot: int;
  user: string;
  credentials: string;
  last_heartbeat_time: Time_ns.t;
}

let register_heartbeat t hb cr = 
  {t with last_heartbeat_time = hb; credentials = cr}

open Ppx_jane  
module Logon = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      user: string;
      mutable credentials: string;
    }
  [@@deriving fields]
end


let logon =
  { Logon.
    session_id = "26685";
    time = Time_ns.of_string_with_utc_offset "2017-07-21 10:11:45Z";
    user = "yminsky";
    credentials = "Xy2d9W";
  }


let show_field field to_string record =
    let name = Field.name field in
    let field_string = to_string (Field.get field record) in
    name ^ ": " ^ field_string

let session_id = show_field  Logon.Fields.user Fn.id logon
let time = show_field Logon.Fields.time Time_ns.to_string_utc logon


let print_logon logon =
  let print to_string field =
    printf "%s\n" (show_field field to_string logon)
  in
  Logon.Fields.iter
    ~session_id:(print Fn.id)
    ~time:(print Time_ns.to_string_utc)
    ~user:(print Fn.id)
    ~credentials:(print Fn.id)