open Chat

let run_client host port =
  let%lwt addresses = Lwt_unix.getaddrinfo host port [] in
  let sockaddr = (List.hd addresses).ai_addr in
  let state = Messaging.State in
  Lwt_io.with_connection sockaddr @@ Messaging.connection_handler state

let () =
  let open Arg in
  let host = ref "localhost" in
  let port = ref "14827" in
  let specs =
    [ ("-h", Set_string host, "Server host. Default: " ^ !host);
      ("-p", Set_string port, "Server port. Default: " ^ !port) ]
  in
  let usage = "chat-client [options]" in
  parse specs (fun arg -> raise @@ Bad ("Unexpected argument: " ^ arg)) usage;
  Lwt_main.run (run_client !host !port)
