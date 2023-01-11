open Odoc_model
open Paths
open Utils.OptionMonad

module OfL = Component.Of_Lang

let rec sig_of_simple_expansion : Component.ModuleType.simple_expansion -> _ =
  function
  | Signature sg -> sg
  | Functor (_, res) -> sig_of_simple_expansion res

let lookup_parent env parent =
  let empty = OfL.empty () in
  let parent = OfL.module_path empty parent in
  Tools.resolve_module ~mark_substituted:false ~add_canonical:false env parent
  |> of_result
  >>= fun (r_cpath, m) ->
  let r_path = Lang_of.Path.resolved_module (Lang_of.empty ()) r_cpath in
  let id = Path.Resolved.Module.identifier r_path in
  let m = Component.Delayed.get m in
  Tools.expansion_of_module env m |> of_result >>= fun exp ->
  Expand_tools.handle_expansion env (id :> Identifier.Signature.t) exp
  |> of_result
  >>= fun (_, exp) -> Some (sig_of_simple_expansion exp)

let of_ftype =
  let open Component in
  function
  | `FType (_, type_) -> Some type_.TypeDecl.locs
  | `FClass (_, class_) -> Some class_.Class.locs
  | `FClassType (_, ct) -> Some ct.ClassType.locs
  | `FType_removed _ -> None

let lookup_loc env = function
  | `Value (parent, name) ->
      lookup_parent env parent >>= fun sg ->
      Find.value_in_sig_unambiguous sg name >>= fun (`FValue (_, value)) ->
      Some value.locs
  | `Type p ->
      let p = OfL.type_path (OfL.empty ()) p in
      Tools.resolve_type env ~add_canonical:false p |> of_result
      >>= fun (_, res) -> of_ftype res
  | `Module p ->
      let p = OfL.module_path (OfL.empty ()) p in
      Tools.resolve_module env ~mark_substituted:false ~add_canonical:false p
      |> of_result
      >>= fun (_, m) ->
      let m = Component.Delayed.get m in
      m.locs
  | `ModuleType p ->
      let p = OfL.module_type_path (OfL.empty ()) p in
      Tools.resolve_module_type env ~mark_substituted:false ~add_canonical:false
        p
      |> of_result
      >>= fun (_, mt) -> mt.locs
  | `Extension (parent, name) ->
      lookup_parent env parent >>= fun sg ->
      Find.extension_in_sig sg name >>= fun (`FExt (_, cons)) -> Some cons.locs
  | `Class (parent, name) ->
      lookup_parent env parent >>= fun sg ->
      Find.class_in_sig_unambiguous sg name >>= of_ftype
  | `ClassType p ->
      let p = OfL.class_type_path (OfL.empty ()) p in
      Tools.resolve_class_type env p |> of_result >>= fun (_, res) ->
      of_ftype res
