type unix_fd = {
    raw : Unix.file_descr;
    mutable needs_close : bool;
  }

let close fd =
  assert (fd.needs_close);
  Unix.close fd.raw;
  fd.needs_close <- false

let ensure_closed_unix fd =
  if fd.needs_close then close fd

let with_pipe_from_child fn =
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.clear_close_on_exec w;
  let r = { raw = r; needs_close = true } in
  let w = { raw = w; needs_close = true } in
  Fun.protect
    (fun () -> fn ~r ~w)
    ~finally:(fun () ->
      ensure_closed_unix r;
      ensure_closed_unix w;
    )

let with_pipe_to_child fn =
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.clear_close_on_exec r;
  let r = { raw = r; needs_close = true } in
  let w = { raw = w; needs_close = true } in
  Fun.protect
    (fun () -> fn ~r ~w)
    ~finally:(fun () ->
      ensure_closed_unix r;
      ensure_closed_unix w;
    )

let with_pipe_between_children fn =
  let r, w = Unix.pipe ~cloexec:true () in
  let r = { raw = r; needs_close = true } in
  let w = { raw = w; needs_close = true } in
  Fun.protect
    (fun () -> fn ~r ~w)
    ~finally:(fun () ->
      ensure_closed_unix r;
      ensure_closed_unix w;
    )
