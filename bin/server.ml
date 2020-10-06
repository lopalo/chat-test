open Chat
module Msg = Messaging

let establish_server state host port =
  let open Lwt_io in
  let listening_address =
    Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port)
  in
  let buffer_size = default_buffer_size () in
  let rec accept_loop () =
    let listening_socket =
      Lwt_unix.socket
        (Unix.domain_of_sockaddr listening_address)
        Unix.SOCK_STREAM 0
    in
    Lwt_unix.setsockopt listening_socket Unix.SO_REUSEADDR true;
    let%lwt () = Lwt_unix.bind listening_socket listening_address in
    Lwt_unix.listen listening_socket 0;
    let%lwt client_socket, _ = Lwt_unix.accept listening_socket in
    let%lwt () = Lwt_unix.close listening_socket in
    let close = lazy (Lwt_unix.close client_socket) in
    let input_channel =
      of_fd
        ~buffer:(Lwt_bytes.create buffer_size)
        ~mode:input
        ~close:(fun () -> Lazy.force close)
        client_socket
    in
    let output_channel =
      of_fd
        ~buffer:(Lwt_bytes.create buffer_size)
        ~mode:output
        ~close:(fun () -> Lazy.force close)
        client_socket
    in
    let%lwt () = Msg.connection_handler state (input_channel, output_channel) in
    accept_loop ()
  in
  accept_loop ()

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
  let state = Msg.State in
  let server = establish_server state !host !port in
  Lwt_main.run server
