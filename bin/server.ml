open Chat
module Msg = Messaging

let run_server host port =
  let state = Msg.State in
  let address = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let mutex = Lwt_mutex.create () in
  let conn_handler _ connection =
    (* TODO: instead of the mutex, accept next connection only after the current one terminates *)
    Lwt_mutex.with_lock mutex (fun () ->
        Msg.connection_handler state connection)
  in
  Lwt_io.establish_server_with_client_address address conn_handler

(* TODO: delete *)
let rec forever () = Lwt.bind (Lwt_unix.sleep 60.) (fun _ -> forever ())

let () =
  let open Arg in
  let host = ref "127.0.0.1" in
  let port = ref 14827 in
  let specs =
    [ ("-h", Set_string host, "Listening IP address. Default: " ^ !host);
      ("-p", Set_int port, "Listening port. Default: " ^ Int.to_string !port) ]
  in
  let usage = "chat-server [options]" in
  parse specs (fun arg -> raise @@ Bad ("Unexpected argument: " ^ arg)) usage;
  let _server = run_server !host !port in
  Lwt_main.run (forever ())
