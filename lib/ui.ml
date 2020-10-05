let write str = Lwt_io.(write stdout str)

let write_line line =
  Lwt_io.(
    let%lwt () = write_line stdout line in
    flush stdout)

let read_line () = Lwt_io.(read_line stdin)
