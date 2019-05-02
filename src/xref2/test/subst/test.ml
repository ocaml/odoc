    open Odoc_xref2
open Odoc_xref_test

let filter_map f m =
    let rec inner = function
    | x::xs -> begin match f x with | Some x' -> x'::inner xs | None -> inner xs end
    | [] -> []
    in inner m 

let resolve_module_name sg name =
    let rec check = function
        | Component.Signature.Module (id, _r, _m) :: _rest when Ident.Name.module_ id = name -> id
        | _ :: rest -> check rest
        | [] -> failwith "Unknown"
    in check sg.Component.Signature.items
(* Module substitution test *)

let name = "Module substitution"
let description = {|
This test substitutes one module for another. We substitute
SubTargets in place of SubstituteMe, so the result expected is that
the equations for t, u and v point to SubTargets rather than SubstituteMe
|}

let test_data = {|

module SubstituteMe : sig
    type t
    type u
    type v
end

module SubTargets : sig
    type t
    type u
    type v
end

module S : sig
    open SubstituteMe
    type tt = t
    type uu = u
    type vv = v
end

|}


let module_substitution () =
    let _, _, sg = Common.model_of_string test_data in

    let c = Component.Of_Lang.(signature empty sg) in

    let subst_idents_mod = resolve_module_name c "SubstituteMe" in
    let subst_targets_mod = resolve_module_name c "SubTargets" in

    let subst = Subst.add_module subst_idents_mod (`Local subst_targets_mod) Subst.identity in

    let m = Component.Find.module_in_sig c "S" in

    let m' = Subst.module_ subst m in

    let open Format in
    fprintf std_formatter "%s\n%s\n%!" name description;
    fprintf std_formatter "BEFORE\n======\n%!";
    fprintf std_formatter "S%a\n\n%!" Component.Fmt.module_ m;
    fprintf std_formatter "AFTER \n======\n%!";
    fprintf std_formatter "S%a\n\n%!" Component.Fmt.module_ m'

let _ =
    module_substitution ()