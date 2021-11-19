let manifest =
  {|
(File
 (hello.txt
  724e1e4079191f086c3a2bcc15133b4309ce5125dbb31c5443262599ca992601))
|}

let manifest = Parsexp.Single.parse_string_exn manifest |> Manifest.t_of_sexp

let op = `Copy_item (manifest, "/hello.txt")

let user : Obuilder_spec.user =
  Obuilder_spec.(`Windows { name = "ContainerAdministrator" })

let transform_tar ~in_tar:from_tar ~out_tar:to_untar =
  Logs.debug (fun f -> f "TRANSFORM START");
  let r =
    match op with
    | `Copy_items (src_manifest, dst_dir) ->
        Tar_transfer.transform_files ~from_tar ~src_manifest ~dst_dir ~user
          ~to_untar
    | `Copy_item (src_manifest, dst) ->
        Tar_transfer.transform_file ~from_tar ~src_manifest ~dst ~user ~to_untar
  in
  r

let rec write_all fd buf ofs len =
  assert (len >= 0);
  if len = 0 then Lwt.return_unit
  else
    let%lwt n = Lwt_unix.write fd buf ofs len in
    write_all fd buf (ofs + n) (len - n)

let transform_noop ~in_tar ~out_tar =
  let max_chunk_size = 4096 in
  let buf = Bytes.create max_chunk_size in
  let rec aux () =
    match%lwt Lwt_unix.read in_tar buf 0 max_chunk_size with
    | 0 -> Lwt.return_unit
    | n ->
        write_all out_tar buf 0 n;%lwt
        aux ()
  in
  aux ()

let transform =
  transform_tar

let ensure_closed_lwt fd =
  if Lwt_unix.state fd = Lwt_unix.Closed then Lwt.return_unit
  else Lwt_unix.close fd

let with_pipe_to_child fn =
  let r, w = Lwt_unix.pipe_out ~cloexec:true () in
  Unix.clear_close_on_exec r;
  (fn ~r ~w) [%lwt.finally (* Unix.close r; *) ensure_closed_lwt w]

let with_pipe_from_child fn =
  let r, w = Lwt_unix.pipe_in ~cloexec:true () in
  Unix.clear_close_on_exec w;
  (fn ~r ~w) [%lwt.finally (* Unix.close w; *) ensure_closed_lwt r]

let pp_cmd = Fmt.hbox Fmt.(list ~sep:sp (quote string))

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let exec ~stdin ~stdout argv =
  let pp f = pp_cmd f argv in
  match%lwt Lwt_process.exec ~stdin ~stdout ("", Array.of_list argv) with
  | Unix.WEXITED 0 -> Lwt.return_unit
  | Unix.WEXITED x -> Fmt.failwith "%t exited with code %d" pp x
  | Unix.WSIGNALED x -> Fmt.failwith "%t failed with signal %d" pp x
  | Unix.WSTOPPED x -> Fmt.failwith "%t stopped with signal %a" pp pp_signal x

let cpus = string_of_int 6
let volume     = "obuilder-d16cf18-volume"
let base_image = "obuilder-d16cf18-image-6c21b8e220f828e8b8a75360ed0cc09d56c672701104cec56b270491af575173"
let tmp_image  = "obuilder-d16cf18-image-tmp-535c2e30461f6205107d3525bddae6ec3f2c70bb6ae126ce66cb43b034513e66"
let container  = "obuilder-d16cf18-container-535c2e30461f6205107d3525bddae6ec3f2c70bb6ae126ce66cb43b034513e66"

let strf = Printf.sprintf

let main =
  let input ~stdout =
    let files_from_content =
      Manifest.to_from_files ~null:true manifest |> Bytes.of_string
    in
    let len = Bytes.length files_from_content in
    let%lwt n = Lwt_unix.write stdout files_from_content 0 len in
    assert%lwt (len = n)
  in

  let step1 ~stdout =
    with_pipe_to_child @@ fun ~r ~w ->
    let proc : unit Lwt.t =
      exec ~stdin:(`FD_move r) ~stdout
        [
          "docker";
          "run";
          "-i";
          "--rm";
          "--cpus";
          cpus;
          "--isolation";
          "hyperv";
          "--hostname";
          "builder";
          "--workdir";
          "C:/";
          "--entrypoint";
          strf "C://%s/tar.exe" volume;
          "--user";
          "ContainerAdministrator";
          "--mount";
          strf "type=volume,src=%s,dst=C:/%s,readonly" volume volume;
          base_image;
          "-cf-";
          "--format=gnu";
          "--directory";
          "C:/";
          "--verbatim-files-from";
          "--null";
          "--absolute-names";
          "--files-from=-";
        ]
    in
    let send = (input ~stdout:w) [%lwt.finally Lwt_unix.close w] in
    let%lwt () = proc in
    send
  in

  let step2 ~stdout =
    with_pipe_from_child @@ fun ~r ~w ->
    let proc = transform ~in_tar:r ~out_tar:stdout in
    let send =
      (step1 ~stdout:(`FD_move w)) [%lwt.finally Lwt.return (Unix.close w)]
    in
    let%lwt () = proc in
    send
  in

  let untar () =
    with_pipe_to_child @@ fun ~r ~w ->
    let proc =
      exec ~stdin:(`FD_move r) ~stdout:`Keep
        [
          "docker";
          "run";
          "-i";
          "--name";
          container;
          "--cpus";
          cpus;
          "--isolation";
          "hyperv";
          "--hostname";
          "builder";
          "--workdir";
          "C:/";
          "--entrypoint";
          "tar";
          "--user";
          "ContainerAdministrator";
          "--mount";
          strf "type=volume,src=%s,dst=C:/%s,readonly" volume volume;
          tmp_image;
          "-xf";
          "-";
          "--verbose";
        ]
    in
    let send = (step2 ~stdout:w) [%lwt.finally Lwt_unix.close w] in
    let%lwt () = proc in
    send
  in

  untar ()

let main style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  let pid =
    Unix.create_process "docker"
      [|
        "docker";
        "rm";
        strf "/%s" container;
      |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  ignore (Unix.waitpid [] pid);
  Lwt_main.run main

open Cmdliner

let main =
  ( Term.(const main $ Fmt_cli.style_renderer () $ Logs_cli.level ()),
    Term.info "main" )

let () = Term.(exit @@ eval main)
