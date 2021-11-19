let ( / ) = Filename.concat

let level = Tar.Header.GNU

let remove_leading_slashes = Astring.String.drop ~sat:(( = ) '/')

let transform ~user fname hdr =
  (* Make a copy to erase unneeded data from the tar headers. *)
  let hdr' =
    Tar.Header.(
      make ~file_mode:hdr.file_mode ~mod_time:hdr.mod_time hdr.file_name
        hdr.file_size)
  in
  let hdr' =
    match user with
    | `Unix user ->
        {
          hdr' with
          Tar.Header.user_id = user.Obuilder_spec.uid;
          group_id = user.gid;
        }
    | `Windows user when user.Obuilder_spec.name = "ContainerAdministrator" ->
        (* https://cygwin.com/cygwin-ug-net/ntsec.html#ntsec-mapping *)
        let id =
          let x = 93 and rid = 1 in
          (0x1000 * x) + rid
        in
        {
          hdr' with
          user_id = id;
          group_id = id;
          uname = user.name;
          gname = user.name;
        }
    | `Windows _ -> hdr'
  in
  match hdr.Tar.Header.link_indicator with
  | Normal ->
      {
        hdr' with
        file_mode = (if hdr.file_mode land 0o111 <> 0 then 0o755 else 0o644);
        file_name = fname hdr.file_name;
      }
  | Symbolic ->
      {
        hdr' with
        file_mode = 0o777;
        file_name = fname hdr.file_name;
        link_indicator = hdr.link_indicator;
        link_name = hdr.link_name;
      }
  | Directory ->
      { hdr' with file_mode = 0o755; file_name = fname hdr.file_name ^ "/" }
  | _ -> Fmt.invalid_arg "Unsupported file type"

let rec map_transform ~dst transformations = function
  | `File (src, _) ->
      let dst = dst / Filename.basename src in
      Hashtbl.add transformations src dst
  | `Symlink (src, _) ->
      let dst = dst / Filename.basename src in
      Hashtbl.add transformations src dst
  | `Dir (src, items) ->
      let dst = dst / Filename.basename src in
      Hashtbl.add transformations src dst;
      Logs.debug (fun f -> f "Copy dir %S -> %S@." src dst);
      List.iter (map_transform ~dst transformations) items

and transform_files ~from_tar ~src_manifest ~dst_dir ~user ~to_untar =
  let dst = remove_leading_slashes dst_dir in
  let transformations = Hashtbl.create ~random:true 64 in
  List.iter (map_transform ~dst transformations) src_manifest;
  let fname file_name =
    match Hashtbl.find transformations file_name with
    | exception Not_found ->
        Fmt.failwith "Could not find mapping for %s" file_name
    | file_name -> file_name
  in
  Tar_lwt_unix.Archive.transform ~level (transform ~user fname) from_tar
    to_untar

let transform_file ~from_tar ~src_manifest ~dst ~user ~to_untar =
  let dst = remove_leading_slashes dst in
  let transformations = Hashtbl.create ~random:true 1 in
  let map_transform = function
    | `File (src, _) -> Hashtbl.add transformations src dst
    | `Symlink (src, _) -> Hashtbl.add transformations src dst
    | `Dir (src, items) ->
        Hashtbl.add transformations src dst;
        Logs.debug (fun f -> f "Copy dir %S -> %S@." src dst);
        List.iter (map_transform ~dst transformations) items
  in
  map_transform src_manifest;
  let fname file_name =
    match Hashtbl.find transformations file_name with
    | exception Not_found ->
        Fmt.failwith "Could not find mapping for %s" file_name
    | file_name -> file_name
  in
  Tar_lwt_unix.Archive.transform ~level
    (fun hdr ->
      let hdr' = transform ~user fname hdr in
      Logs.debug (fun f ->
          f "Copying %s -> %s@." hdr.Tar.Header.file_name
            hdr'.Tar.Header.file_name);
      hdr')
    from_tar to_untar
