open Lwt.Infix

type state = {mutable current_message_id : Int64.t}

type message =
  | Message of
      { id : Int64.t;
        timestamp : Int64.t;
        payload : string }
  | Ack of
      { id : Int64.t;
        message_timestamp : Int64.t }

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

let print_line line =
  Lwt_io.(
    let%lwt () = write_line stdout line in
    flush stdout)

let print_prompt {current_message_id} =
  Lwt_io.(
    let%lwt () = write stdout (fmt "#%Li>> " current_message_id) in
    flush stdout)

let read_line () = Lwt_io.(read_line stdin)

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

let rec message_reader state input_channel output_channel =
  let%lwt () =
    match%lwt read_message input_channel with
    | Message {payload; id; timestamp} ->
        let%lwt () =
          write_message output_channel (Ack {id; message_timestamp = timestamp})
        in
        print_line (fmt "\nReceived: " ^ payload)
    | Ack {id; message_timestamp} ->
        let rtt_ns = Int64.sub (Mtime_clock.elapsed_ns ()) message_timestamp in
        let rtt_ms = Int64.to_float rtt_ns /. 1_000_000.0 in
        print_line (fmt "Ack #%Li, RTT: %.2f ms" id rtt_ms)
  in
  let%lwt () = print_prompt state in
  message_reader state input_channel output_channel

let rec message_writer state channel =
  let%lwt payload = read_line () in
  let id = state.current_message_id in
  state.current_message_id <- Int64.succ id;
  let timestamp = Mtime_clock.elapsed_ns () in
  let msg = Message {id; timestamp; payload} in
  let%lwt () = write_message channel msg in
  let%lwt () = print_line (fmt "Sent #%Li" id) in
  message_writer state channel

let make_state () = {current_message_id = Int64.zero}

let connection_handler state (input, output) =
  let%lwt () = print_line "Connected" in
  let%lwt () = print_prompt state in
  let writer =
    try%lwt message_writer state output with
    | Unix.Unix_error (Unix.EBADF, _, _)
    | Unix.Unix_error (Unix.ENOTCONN, _, _)
    | Unix.Unix_error (Unix.EPIPE, _, _)
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _)
    | Unix.Unix_error (Unix.ECONNRESET, _, _)
    | Unix.Unix_error (Unix.ECONNABORTED, _, _) ->
        Lwt.return_unit
  in
  let%lwt () =
    try%lwt message_reader state input output with
    | End_of_file
    | Unix.Unix_error (Unix.EBADF, _, _)
    | Unix.Unix_error (Unix.ENOTCONN, _, _)
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _)
    | Unix.Unix_error (Unix.ECONNRESET, _, _)
    | Unix.Unix_error (Unix.ECONNABORTED, _, _) ->
        Lwt.return_unit
  in
  Lwt.cancel writer; print_line "Disconnected"
