open Odoc_utils
open ResultMonad

let compile ~parent_id ~name ~output_dir =
  let open Odoc_model in
  let parent_id =
    match Compile.mk_id parent_id with
    | Some s -> Ok s
    | None -> Error (`Msg "parent-id cannot be empty when compiling assets.")
  in
  parent_id >>= fun parent_id ->
  let id =
    Paths.Identifier.Mk.asset_file
      ((parent_id :> Paths.Identifier.Page.t), Names.AssetName.make_std name)
  in
  let directory =
    Compile.path_of_id output_dir (Some parent_id)
    |> Fpath.to_string |> Fs.Directory.of_string
  in
  let name = "asset-" ^ name ^ ".odoc" in
  let output = Fs.File.create ~directory ~name in
  let digest = Digest.string name in
  let root =
    Root.
      {
        id = (id :> Paths.Identifier.OdocId.t);
        digest;
        file = Odoc_file.asset name;
      }
  in
  let asset = Lang.Asset.{ name = id; root } in
  Ok (Odoc_file.save_asset output ~warnings:[] asset)
