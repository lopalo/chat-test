open Lwt.Infix

let fmt = Printf.sprintf

let write_uint8 channel i = Char.unsafe_chr i |> Lwt_io.write_char channel

let read_uint8 channel = Lwt_io.read_char channel >|= Char.code

let write_int64 = Lwt_io.BE.write_int64

let read_int64 = Lwt_io.BE.read_int64

let write_payload channel payload =
  let%lwt () = String.length payload |> Int64.of_int |> write_int64 channel in
  Lwt_io.write channel payload

let read_payload channel =
  let%lwt length = read_int64 channel >|= Int64.to_int in
  let buffer = Bytes.create length in
  Lwt_io.read_into_exactly channel buffer 0 length
  >|= fun () -> Bytes.unsafe_to_string buffer

type state = State

type message =
  | Message of
      { id : Int64.t;
        timestamp : Int64.t;
        payload : string }
  | Ack of
      { id : Int64.t;
        message_timestamp : Int64.t }

let write_message channel msg =
  Lwt_io.atomic
    (fun channel ->
      match msg with
      | Message {id; timestamp; payload} ->
          let%lwt () = write_uint8 channel 0 in
          let%lwt () = write_int64 channel id in
          let%lwt () = write_int64 channel timestamp in
          let%lwt () = write_payload channel payload in
          Lwt.return_unit
      | Ack {id; message_timestamp} ->
          let%lwt () = write_uint8 channel 1 in
          let%lwt () = write_int64 channel id in
          let%lwt () = write_int64 channel message_timestamp in
          Lwt.return_unit)
    channel

let read_message channel =
  match%lwt read_uint8 channel with
  | 0 ->
      let%lwt id = read_int64 channel in
      let%lwt timestamp = read_int64 channel in
      let%lwt payload = read_payload channel in
      Lwt.return (Message {id; timestamp; payload})
  | 1 ->
      let%lwt id = read_int64 channel in
      let%lwt message_timestamp = read_int64 channel in
      Lwt.return (Ack {id; message_timestamp})
  | tag -> Lwt.fail (Failure ("Unknown message tag: " ^ string_of_int tag))

let rec message_reader input_channel output_channel =
  let%lwt () =
    match%lwt read_message input_channel with
    | Message {payload; id; timestamp} ->
        let%lwt () =
          write_message output_channel (Ack {id; message_timestamp = timestamp})
        in
        let%lwt () = Ui.write_line "" in
        Ui.write_line (fmt "Received: " ^ payload)
    | Ack {id; message_timestamp} ->
        let open Int64 in
        let rtt_ns = sub (Mtime_clock.elapsed_ns ()) message_timestamp in
        let rtt_ms = div rtt_ns (of_int 1_000_000) in
        Ui.write_line (fmt "Ack #%Li, RTT: %Li ms" id rtt_ms)
  in
  message_reader input_channel output_channel

let rec message_writer channel id =
  let%lwt payload = Ui.read_line () in
  let%lwt () = Ui.write_line (fmt "Sent #%Li" id) in
  let timestamp = Mtime_clock.elapsed_ns () in
  let msg = Message {id; timestamp; payload} in
  let%lwt () = write_message channel msg in
  message_writer channel (Int64.succ id)

let connection_handler _state (input, output) =
  let%lwt () = Ui.write_line "Connected" in
  let writer =
    try%lwt message_writer output Int64.zero with
    | Unix.Unix_error (Unix.EBADF, _, _)
    | Unix.Unix_error (Unix.ENOTCONN, _, _)
    | Unix.Unix_error (Unix.EPIPE, _, _)
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _)
    | Unix.Unix_error (Unix.ECONNRESET, _, _)
    | Unix.Unix_error (Unix.ECONNABORTED, _, _) ->
        Lwt.return_unit
  in
  let%lwt () =
    try%lwt message_reader input output with
    | End_of_file
    | Unix.Unix_error (Unix.EBADF, _, _)
    | Unix.Unix_error (Unix.ENOTCONN, _, _)
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _)
    | Unix.Unix_error (Unix.ECONNRESET, _, _)
    | Unix.Unix_error (Unix.ECONNABORTED, _, _) ->
        Lwt.return_unit
  in
  Lwt.cancel writer;
  Ui.write_line "Disconnected"
