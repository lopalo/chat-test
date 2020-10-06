type state

val make_state : unit -> state

val connection_handler :
  state -> Lwt_io.input_channel * Lwt_io.output Lwt_io.channel -> unit Lwt.t
