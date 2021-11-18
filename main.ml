let manifest = {|
(File
 (hello.txt
  724e1e4079191f086c3a2bcc15133b4309ce5125dbb31c5443262599ca992601))
|}

let manifest = Parsexp.Single.parse_string_exn manifest |> Manifest.t_of_sexp
let op = `Copy_item (manifest, "/hello.txt")
let user : Obuilder_spec.user = Obuilder_spec.(`Windows {name = "ContainerAdministrator"})

let transform op ~user ~in_tar:from_tar ~out_tar:to_untar =
  Logs.debug (fun f -> f "TRANSFORM START");
  let r = match op with
  | `Copy_items (src_manifest, dst_dir) ->
    Tar_transfer.transform_files ~from_tar ~src_manifest ~dst_dir ~user ~to_untar
  | `Copy_item (src_manifest, dst) ->
    Tar_transfer.transform_file ~from_tar ~src_manifest ~dst ~user ~to_untar
  in
  r

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let tarball_from_build ~files_from ~tar_out =
  Logs.debug (fun f -> f "TARBALL FROM BUILD START");
  let pid = Unix.create_process "docker" [|"docker"; "run"; "-i"; "--rm"; "--cpus"; "6"; "--isolation"; "hyperv"; "--hostname"; "builder"; "--workdir"; "C:/"; "--entrypoint"; "C://obuilder-d16cf18-volume/tar.exe"; "--user"; "ContainerAdministrator"; "--mount"; "type=volume,src=obuilder-d16cf18-volume,dst=C:/obuilder-d16cf18-volume,readonly"; "obuilder-d16cf18-image-6c21b8e220f828e8b8a75360ed0cc09d56c672701104cec56b270491af575173"; "-cf-"; "--format=gnu"; "--directory"; "C:/"; "--verbatim-files-from"; "--null"; "--absolute-names"; "--files-from=-"|] files_from.Os.raw tar_out.Os.raw Unix.stderr in
  Os.ensure_closed_unix files_from;
  Os.ensure_closed_unix tar_out;
  pid

let untar ~untar_in =
  Logs.debug (fun f -> f "UNTAR START");
  let pid = Unix.create_process "docker" [|"docker"; "run"; "-i"; "--name"; "obuilder-d16cf18-container-535c2e30461f6205107d3525bddae6ec3f2c70bb6ae126ce66cb43b034513e66"; "--cpus"; "6"; "--isolation"; "hyperv"; "--hostname"; "builder"; "--workdir"; "C:/"; "--entrypoint"; "tar"; "--user"; "ContainerAdministrator"; "--mount"; "type=volume,src=obuilder-d16cf18-volume,dst=C:/obuilder-d16cf18-volume,readonly"; "obuilder-d16cf18-image-tmp-535c2e30461f6205107d3525bddae6ec3f2c70bb6ae126ce66cb43b034513e66"; "-xf"; "-"; "--verbose"|] untar_in.Os.raw Unix.stdout Unix.stderr in
  Os.ensure_closed_unix untar_in;
  pid

let cleanup () =
  let pid = Unix.create_process "docker" [|"docker"; "rm"; "/obuilder-d16cf18-container-535c2e30461f6205107d3525bddae6ec3f2c70bb6ae126ce66cb43b034513e66"|] Unix.stdin Unix.stdout Unix.stderr in
  ignore (Unix.waitpid [] pid)

let main style_renderer level =
  setup_logs style_renderer level;

  let max_chunk_size = 4096 in
  let _buf = Bytes.create max_chunk_size in
  Os.with_pipe_to_child @@ fun ~r:files_from ~w:files_from_out ->
  let files_from_content = Manifest.to_from_files ~null:true manifest |> Bytes.of_string  in
  Os.with_pipe_from_child @@ fun ~r:tar_in ~w:tar_out ->
  Os.with_pipe_to_child @@ fun ~r:untar_in ~w:to_untar ->
  let pid = untar ~untar_in in
  Fun.protect (fun () -> let _pid = tarball_from_build ~files_from ~tar_out in ())
    ~finally:(fun () -> Os.ensure_closed_unix tar_out; Logs.debug (fun f -> f "TARBALL FROM BUILD END"));
  Fun.protect (fun () ->
      let len = Bytes.length files_from_content in
      let n = Unix.write files_from_out.raw files_from_content 0 len in
      assert (n = len))
    ~finally:(fun () -> Os.ensure_closed_unix files_from_out; Logs.debug (fun f -> f "FILES_FROM END"));
  Fun.protect (fun () -> transform op ~user ~in_tar:tar_in.raw ~out_tar:to_untar.raw)
    ~finally:(fun () -> Os.ensure_closed_unix to_untar; Logs.debug (fun f -> f "TRANSFORM END"));
  begin match (let x = Unix.waitpid [] pid in Logs.debug (fun f -> f "UNTAR END"); x) with
  | _, Unix.WEXITED n -> Printf.printf "Untar process exited with %d." n
  | _, Unix.WSIGNALED n -> Printf.printf "Untar process signaled with %d." n
  | _, Unix.WSTOPPED n -> Printf.printf "Untar process stopped with %d." n
  end;
  cleanup ()

open Cmdliner

let main =
  Term.(const main $ Fmt_cli.style_renderer () $ Logs_cli.level ()),
  Term.info "main"

let () = Term.(exit @@ eval main)
