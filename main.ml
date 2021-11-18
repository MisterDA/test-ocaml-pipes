let transform op ~user ~from_tar ~to_untar =
  Logs.debug (fun f -> f "TRANSFORM START");
  let r = match op with
  | `Copy_items (src_manifest, dst_dir) ->
    Tar_transfer.transform_files ~from_tar ~src_manifest ~dst_dir ~user ~to_untar
  | `Copy_item (src_manifest, dst) ->
    Tar_transfer.transform_file ~from_tar ~src_manifest ~dst ~user ~to_untar
  in
  Logs.debug (fun f -> f "TRANSFORM END");
  r

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let tarball_from_build ~files_from ~tar_out =
  let pid = Unix.create_process "docker" [|"docker"; "run"; "-i"; "--rm"; "--cpus"; "6"; "--isolation"; "hyperv"; "--hostname"; "builder"; "--workdir"; "C:/"; "--entrypoint"; "C://obuilder-d16cf18-volume/tar.exe"; "--user"; "ContainerAdministrator"; "--mount"; "type=volume,src=obuilder-d16cf18-volume,dst=C:/obuilder-d16cf18-volume,readonly"; "obuilder-d16cf18-image-6c21b8e220f828e8b8a75360ed0cc09d56c672701104cec56b270491af575173"; "-cf-"; "--format=gnu"; "--directory"; "C:/"; "--verbatim-files-from"; "--null"; "--absolute-names"; "--files-from=-"|] files_from.Os.raw tar_out.Os.raw Unix.stderr in
  Os.ensure_closed_unix files_from;
  Os.ensure_closed_unix tar_out;
  pid

let untar ~tar_in =
  let pid = Unix.create_process "docker" [|"docker"; "run"; "-i"; "--name"; "obuilder-d16cf18-container-535c2e30461f6205107d3525bddae6ec3f2c70bb6ae126ce66cb43b034513e66"; "--cpus"; "6"; "--isolation"; "hyperv"; "--hostname"; "builder"; "--workdir"; "C:/"; "--entrypoint"; "tar"; "--user"; "ContainerAdministrator"; "--mount"; "type=volume,src=obuilder-d16cf18-volume,dst=C:/obuilder-d16cf18-volume,readonly"; "obuilder-d16cf18-image-tmp-535c2e30461f6205107d3525bddae6ec3f2c70bb6ae126ce66cb43b034513e66"; "-xf"; "-"; "--verbose"|] tar_in.Os.raw Unix.stdout Unix.stderr in
  Os.ensure_closed_unix tar_in;
  pid

let main style_renderer level =
  setup_logs style_renderer level;

  let max_chunk_size = 4096 in
  let _buf = Bytes.create max_chunk_size in
  Os.with_pipe_to_child @@ fun ~r:files_from ~w:files_from_out ->
  let files_from_content = Bytes.of_string "C:/hello.txt\000" in
  Os.with_pipe_between_children @@ fun ~r:tar_in ~w:tar_out ->
  let pid = untar ~tar_in in
  Fun.protect (fun () -> let _pid = tarball_from_build ~files_from ~tar_out in ())
    ~finally:(fun () -> Os.ensure_closed_unix tar_out);
  Fun.protect (fun () ->
      let len = Bytes.length files_from_content in
      let n = Unix.write files_from_out.raw files_from_content 0 len in
      assert (n = len))
    ~finally:(fun () -> Os.ensure_closed_unix files_from_out);
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED n -> Printf.printf "Child process exited with %d." n
  | _, Unix.WSIGNALED n -> Printf.printf "Child process signaled with %d." n
  | _, Unix.WSTOPPED n -> Printf.printf "Child process stopped with %d." n

open Cmdliner

let main =
  Term.(const main $ Fmt_cli.style_renderer () $ Logs_cli.level ()),
  Term.info "main"

let () = Term.(exit @@ eval main)
