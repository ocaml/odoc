open Result

(* Example usage of these:

$ dune utop src/xref2/test/lib
utop # open Odoc_xref2;;
utop # open Odoc_xref_test;;
utop # let test_data = "module type M = sig type t end module N : M type u = N.t";;
utop # let id, docs, sg = Common.model_of_string test_data;;
utop # let env = Env.open_signature sg Env.empty;;
utop # let unit = Common.my_compilation_unit id docs sg;
utop # Common.resolve unit
utop # Resolve.signature Env.empty sg

*)


let _ = Toploop.set_paths ()

let cmti_of_string s =
    Odoc_xref2.Tools.reset_caches ();
    let env = Compmisc.initial_env () in
    let l = Lexing.from_string s in
    let p = Parse.interface l in
    Typemod.type_interface
#if OCAML_VERSION >= (4,4,0) && OCAML_VERSION < (4,9,0)
    ""
#endif
    env p;;

let cmt_of_string s =
    let env = Compmisc.initial_env () in
    let l = Lexing.from_string s in
    let p = Parse.implementation l in
#if OCAML_VERSION < (5,2,0)
    Typemod.type_implementation "" "" "" env p
#else
    Typemod.type_implementation (Unit_info.make ~source_file:"" "") env p
#endif

let parent = Odoc_model.Paths.Identifier.Mk.page (None, Odoc_model.Names.PageName.make_std "None")
let id = Odoc_model.Paths.Identifier.Mk.root (Some parent, Odoc_model.Names.ModuleName.make_std "Root")

let root_of_compilation_unit ~package ~hidden ~module_name ~digest =
  ignore(package);
  let file_representation : Odoc_model.Root.Odoc_file.t =
  Odoc_model.Root.Odoc_file.create_unit ~force_hidden:hidden module_name in
  Ok {Odoc_model.Root.id; file = file_representation; digest}

let root =
    let root_result =
        root_of_compilation_unit
            ~package:"nopackage"
            ~hidden:false
            ~module_name:"Root"
            ~digest:"nodigest"
    in
    match root_result with
    | Ok r -> r
    | _ -> failwith "Bad result"

let root_identifier = `Identifier id

let root_module name = Odoc_model.Paths.Identifier.Mk.module_ (id, Odoc_model.Names.ModuleName.make_std name)

let root_pp fmt (_ : Odoc_model.Root.t) = Format.fprintf fmt "Common.root"

let model_of_string str = 
    let cmti = cmti_of_string str in
    Odoc_loader__Cmti.read_interface (Some parent) "Root" cmti

let model_of_string_impl str =
#if OCAML_VERSION < (4,13,0)
    let (cmt,_) = cmt_of_string str in
#else
    let cmt = (cmt_of_string str).structure in
#endif
    Odoc_loader__Cmt.read_implementation (Some parent) "Root" cmt

let signature_of_mli_string str =
    Odoc_xref2.Ident.reset ();
    let _, sg, _ = model_of_string str in
    sg

let string_of_file f =
    let ic = open_in f in
    let buffer = Buffer.create 100 in
    let rec loop () =
        try
            Buffer.add_channel buffer ic 1024;
            loop ()
        with End_of_file ->
            ()
    in loop ();
    close_in ic;
    Buffer.contents buffer

let file_of_string ~filename str =
    let oc = open_out filename in
    Printf.fprintf oc "%s%!" str;
    close_out oc

let list_files path =
    Sys.readdir path |> Array.to_list

let load_cmti filename =
  let make_root = root_of_compilation_unit ~package:"nopackage" ~hidden:false in
  Odoc_loader.read_cmti ~make_root ~filename

let load_cmt filename =
    let make_root = root_of_compilation_unit ~package:"nopackage" ~hidden:false in
    Odoc_loader.read_cmt ~make_root ~filename
  

module Ident = Ident

module LangUtils = struct

    module Lens = struct
        open Odoc_model

        type ('a, 'b) lens =
            { get : 'a -> 'b
            ; set : 'b -> 'a -> 'a }

        type ('a, 'b) prism =
            { preview : 'a -> 'b option
            ; review : 'b -> 'a }

        let option : ('a option, 'a) prism =
            let preview = function | Some x -> Some x | None -> None in
            let review = function x -> Some x in
            {preview; review}
    
        let compose : ('a, 'b) lens -> ('b, 'c) lens -> ('a, 'c) lens =
            fun x y ->
                let get z = y.get (x.get z) in
                let set a z = x.set (y.set a (x.get z)) z in
                {get; set}

        let compose_prism : ('a, 'b) lens -> ('b, 'c) prism -> ('a, 'c) lens =
            fun x y ->
                let get z = x.get z |> y.preview |> function | Some x -> x | None -> raise Not_found in
                let set a z = x.set (y.review a) z in
                {get; set}

        let fst : ('a * 'b, 'a) lens =
            let get (x,_) = x in
            let set a (_, y) = (a, y) in
            {get; set}

        let snd : ('a * 'b, 'b) lens =
            let get (_, y) = y in
            let set a (x, _) = (x, a) in
            {get; set}
 
        let hd : ('a list, 'a) prism =
            let preview = function
                | x::_ -> Some x
                | _ -> None
            in
            let review x = [x]
            in
            { review ; preview }

        let nth n : ('a list, 'a) prism =
            let preview l = try Some (List.nth l n) with _ -> None in
            let review x = [x]
            in { review; preview }

        let (|--) = compose

        let (|-~) = compose_prism

        let get lens x = lens.get x
        let set lens y x = lens.set y x

        let name_of_id = Paths.Identifier.name

        module Signature = struct
            open Lang.Signature
            let module_ : string -> (t, Lang.Module.t) lens = fun name ->
                let module M = Lang.Module in
                let get sg =
                    let rec inner = function
                    | [] -> raise Not_found
                    | (Module (_r,m'))::_xs when name_of_id m'.M.id = name ->
                        m'
                    | _::xs -> inner xs
                    in inner sg.items
                in
                let set m sg =
                    let rec inner = function
                        | [] -> raise Not_found
                        | (Module (r, m'))::xs when name_of_id m'.M.id = name ->
                            (Module (r, m)::xs)
                        | x::xs -> x :: inner xs
                    in
                    {sg with items = inner sg.items}
                in
                { get; set }

            let module_type : string -> (t, Lang.ModuleType.t) lens = fun name ->
                let module MT = Lang.ModuleType in
                let get sg = 
                    let rec inner = function
                    | [] -> raise Not_found
                    | (ModuleType m')::_xs when name_of_id m'.MT.id = name ->
                        m'
                    | _::xs -> inner xs
                    in inner sg.items
                in
                let set m sg =
                    let rec inner = function
                        | [] -> raise Not_found
                        | (ModuleType m')::xs when name_of_id m'.MT.id = name ->
                            (ModuleType m::xs)
                        | x::xs -> x :: inner xs
                    in
                    {sg with items = inner sg.items}
                in
                { get; set }

            let type_ : string -> (t, Lang.TypeDecl.t) lens = fun name ->
                let module T = Lang.TypeDecl in
                let get sg =
                    let rec inner = function
                    | [] -> raise Not_found
                    | (Type (_,t'))::_xs when name_of_id t'.T.id = name ->
                        t'
                    | _::xs -> inner xs
                    in inner sg.items
                in
                let set t sg =
                    let rec inner = function
                        | [] -> raise Not_found
                        | (Type (r,t'))::xs when name_of_id t'.T.id = name ->
                            (Type (r,t))::xs
                        | x::xs -> x :: inner xs
                    in
                    {sg with items = inner sg.items }
                in
                { get; set }

            let value : string -> (t, Lang.Value.t) lens = fun name ->
                let module V = Lang.Value in
                let get sg =
                    let rec inner = function
                    | [] -> raise Not_found
                    | (Value v) ::_xs when name_of_id v.V.id = name ->
                        v
                    | _::xs -> inner xs
                    in inner sg.items
                in
                let set v sg =
                    let rec inner = function
                        | [] -> raise Not_found
                        | (Value v') :: xs when name_of_id v'.V.id = name ->
                            (Value v)::xs
                        | x::xs -> x :: inner xs
                    in
                    {sg with items = inner sg.items}
                in
                { get; set }
            
            let includes : (t, Lang.Include.t list) lens =
                let get l =
                    List.fold_left (fun acc item ->
                        match item with
                        | (Include i) -> i::acc
                        | _ -> acc) [] l.items |> List.rev
                in
                let set _ _ =
                    raise (Invalid_argument "set includes")
                in
                {get; set }
            end

            module Module = struct
                open Lang.Module

                let id : (t, Paths.Identifier.Module.t) lens =
                    let get m = m.id in
                    let set id t = {t with id} in
                    {get; set}

                let type_ : (t, decl) lens =
                    let get m = m.type_ in
                    let set type_ m = {m with type_} in
                    {get; set}
                
                let decl_moduletype : (decl, Lang.ModuleType.expr) prism =
                    let review x = ModuleType x in
                    let preview = function | ModuleType x -> Some x | _ -> None in
                    {review; preview}
            end

            module Include = struct
                open Lang.Include

                let expansion_sig : (t, Lang.Signature.t) lens =
                    let get x = x.expansion.content in
                    let set x i = {i with expansion = {i.expansion with content=x}} in
                    {get; set}
            end

            module ModuleType = struct
                open Lang.ModuleType

                let id : (t, Paths.Identifier.ModuleType.t) lens =
                    let get mt = mt.id in
                    let set id mt = {mt with id} in
                    {get; set}

                let expr : (t, expr option) lens =
                    let get mt = mt.expr in
                    let set expr mt = {mt with expr} in
                    {get; set}

                let expr_signature : (expr, Lang.Signature.t) prism =
                    let review x = Signature x in
                    let preview = function | Signature x -> Some x | _ -> None in
                    {review; preview}

                let expr_functor : (expr, (Lang.FunctorParameter.t * expr)) prism =
                    let review (x,y) = Functor (x,y) in
                    let preview = function | Functor (x,y) -> Some (x,y) | _ -> None in
                    {review; preview}
            end

            module FunctorParameter = struct
                    open Lang.FunctorParameter

                let id : (parameter, Paths.Identifier.FunctorParameter.t) lens =
                    let get mt = mt.id in
                    let set id mt = {mt with id} in
                    {get; set}

                let expr : (parameter, Lang.ModuleType.expr) lens =
                    let get mt = mt.expr in
                    let set expr mt = {mt with expr} in
                    {get; set}

                let named : (t, parameter) prism =
                  let review x = Named x in
                  let preview = function | Unit -> None | Named p -> Some p in
                  {review; preview}
            end 

            module TypeDecl = struct
                open Lang.TypeDecl

                module Equation = struct
                    open Equation

                    let params : (t, param list) lens =
                        let get t = t.params in
                        let set params t = {t with params} in
                        { get; set }

                    let manifest : (t, Lang.TypeExpr.t option) lens =
                        let get t = t.manifest in
                        let set manifest t = {t with manifest} in
                        { get; set }
                end

                let id : (t, Paths.Identifier.Type.t) lens =
                    let get t = t.id in
                    let set id t = {t with id} in
                    { get; set }
                
                let equation : (t, Lang.TypeDecl.Equation.t) lens =
                    let get t = t.equation in
                    let set equation t = {t with equation} in
                    { get; set }
                
                let representation : (t, Representation.t option) lens =
                    let get t = t.representation in
                    let set representation t = {t with representation} in
                    { get; set }                    
            end
        
            module TypeExpr = struct
                open Lang.TypeExpr

                let constr : (t, (Odoc_model.Paths.Path.Type.t * t list)) prism =
                    let review (x,y) = Constr (x,y) in
                    let preview = function | Constr (x,y) -> Some (x,y) | _ -> None in
                    {review; preview}

            end

    end

    let test =
        let open Lens in
        Signature.type_ "t" |-- TypeDecl.equation |-- TypeDecl.Equation.manifest


    module Lookup = struct
        let module_from_sig : Odoc_model.Lang.Signature.t -> string -> Odoc_model.Lang.Module.t =
            fun sg mname ->
                let rec inner = function
                    | Odoc_model.Lang.Signature.Module (_, m) :: rest -> begin
                        let id = m.Odoc_model.Lang.Module.id in
                        match id.iv with
                        | `Module (_, mname') ->
                            if Odoc_model.Names.ModuleName.to_string mname' = mname
                            then m
                            else inner rest
                        | _ -> inner rest
                        end
                    | _::rest -> 
                        Format.fprintf Format.std_formatter "Found somethine else\n%!";                   
                        inner rest
                    | _ -> raise Not_found
                in 
                inner sg.items
    end

    let sig_of_module : Odoc_model.Lang.Module.t -> Odoc_model.Lang.Signature.t =
        let open Odoc_model.Lang in
        fun x ->
            match x.type_ with
            | Module.ModuleType ModuleType.Signature s -> s
            | _ -> raise Not_found

    module Fmt : sig
        open Odoc_model
        type 'a fmt = Format.formatter -> 'a -> unit

        open Paths
        val identifier : [< Identifier.t_pv] Paths.Identifier.id fmt

        open Lang

        val signature : Signature.t fmt
        val module_decl : Module.decl fmt
        val module_ : Module.t fmt
        val module_type : ModuleType.t fmt
        val u_module_type_expr : ModuleType.U.expr fmt
        val functor_parameter : FunctorParameter.t fmt
        val type_equation : TypeDecl.Equation.t fmt
        val substitution : ModuleType.substitution fmt
        val type_expr : TypeExpr.t fmt
        val resolved_path : Path.Resolved.t fmt
        val path : Path.t fmt
        val model_fragment : Fragment.t fmt
        val model_resolved_fragment : Fragment.Resolved.t fmt
    end = struct
        open Odoc_model.Lang

        type 'a fmt = Format.formatter -> 'a -> unit

        let identifier ppf i =
            Format.fprintf ppf "%a"
                Odoc_xref2.Component.Fmt.model_identifier
                (i :> Odoc_model.Paths.Identifier.t)

        let rec signature ppf sg =
            let open Signature in
            Format.fprintf ppf "@[<v>";
            List.iter (function
                | Module (_,m) ->
                    Format.fprintf ppf
                        "@[<v 2>module %a@]@,"
                        module_ m
                | ModuleType mt ->
                    Format.fprintf ppf
                        "@[<v 2>module type %a@]@,"
                        module_type mt
                | Type (_,t) ->
                    Format.fprintf ppf
                        "@[<v 2>type %a@]@," type_decl t
                | _ ->
                    Format.fprintf ppf
                        "@[<v 2>unhandled signature@]@,") sg.items;
            Format.fprintf ppf "@]"

        and module_decl ppf d =
            let open Module in
            match d with
            | Alias (p, _) ->
                Format.fprintf ppf "= %a" path (p :> Odoc_model.Paths.Path.t)
            | ModuleType mt ->
                Format.fprintf ppf ": %a" module_type_expr mt

        and module_ ppf m =
            Format.fprintf ppf "%a %a" identifier m.id module_decl m.type_

        and module_type ppf mt =
            match mt.expr with
            | Some x -> Format.fprintf ppf "%a = %a" identifier mt.id module_type_expr x
            | None -> Format.fprintf ppf "%a" identifier mt.id

        and u_module_type_expr ppf mt =
            let open ModuleType.U in
            match mt with
            | Path p -> path ppf (p :> Odoc_model.Paths.Path.t)
            | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" signature sg
            | With (subs, expr) -> Format.fprintf ppf "%a with [%a]" u_module_type_expr expr substitution_list subs
            | _ -> Format.fprintf ppf "unhandled module_type_expr"

        and module_type_expr ppf mt =
            let open ModuleType in
            match mt with
            | Path {p_path; _} -> path ppf (p_path :> Odoc_model.Paths.Path.t)
            | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" signature sg
            | With {w_substitutions; w_expr; _} -> Format.fprintf ppf "%a with [%a]" u_module_type_expr w_expr substitution_list w_substitutions
            | Functor (arg, res) -> Format.fprintf ppf "(%a) -> %a" functor_parameter arg module_type_expr res
            | _ -> Format.fprintf ppf "unhandled module_type_expr"

        and functor_parameter ppf x =
            match x with
            | Unit -> ()
            | Named x -> Format.fprintf ppf "%a" functor_parameter_parameter x

        and functor_parameter_parameter ppf x =
            Format.fprintf ppf "%a : %a" identifier x.FunctorParameter.id module_type_expr x.FunctorParameter.expr

        and type_equation ppf t =
            match t.TypeDecl.Equation.manifest with
            | Some m -> Format.fprintf ppf " = %a" type_expr m
            | None -> ()

        and type_decl ppf t =
            let open TypeDecl in
            Format.fprintf ppf "%a%a" identifier t.id type_equation t.equation

        and substitution ppf t =
            let open ModuleType in
            match t with
            | ModuleEq (frag, decl) ->
                Format.fprintf ppf "%a = %a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) module_decl decl
            | ModuleSubst (frag, mpath) ->
                Format.fprintf ppf "%a := %a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) path (mpath :> Odoc_model.Paths.Path.t)
            | TypeEq (frag, decl) ->
                Format.fprintf ppf "%a%a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) type_equation decl
            | TypeSubst (frag, decl) ->
                Format.fprintf ppf "%a:%a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) type_equation decl
            | ModuleTypeEq (frag, decl) ->
                Format.fprintf ppf "%a%a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) module_type_expr decl
            | ModuleTypeSubst (frag, decl) ->
                Format.fprintf ppf "%a:%a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) module_type_expr decl


        and substitution_list ppf l =
            match l with
            | sub :: (_ :: _) as subs -> Format.fprintf ppf "%a; %a" substitution sub substitution_list subs
            | sub :: [] -> Format.fprintf ppf "%a" substitution sub
            | [] -> ()

        and type_expr ppf e =
            let open TypeExpr in
            match e with
            | Var x -> Format.fprintf ppf "%s" x
            | Constr (p,_args) -> path ppf (p :> Odoc_model.Paths.Path.t)
            | _ -> Format.fprintf ppf "unhandled type_expr"

        and resolved_path : Format.formatter -> Odoc_model.Paths.Path.Resolved.t -> unit = fun ppf p ->
            let cast p = (p :> Odoc_model.Paths.Path.Resolved.t) in 
            match p with
            | `Apply (p1, p2) -> Format.fprintf ppf "%a(%a)" resolved_path (cast p1) resolved_path (cast p2)
            | `Identifier p -> Format.fprintf ppf "global(%a)" identifier p
            | `Alias (dest, src) -> Format.fprintf ppf "(%a -> %a)" path (src :> Odoc_model.Paths.Path.t) resolved_path (cast dest)
            | `AliasModuleType (path, realpath) -> Format.fprintf ppf "(%a -> %a)" resolved_path (cast path) resolved_path (cast realpath)
            | `Subst (modty, m) -> Format.fprintf ppf "(%a subst-> %a)" resolved_path (cast modty) resolved_path (cast m)
            | `Module (p, m) -> Format.fprintf ppf "%a.%s" resolved_path (cast p) (Odoc_model.Names.ModuleName.to_string m)
            | `ModuleType (p, mt) -> Format.fprintf ppf "%a.%s" resolved_path (cast p) (Odoc_model.Names.ModuleTypeName.to_string mt)
            | `Type (p, t) -> Format.fprintf ppf "%a.%s" resolved_path (cast p) (Odoc_model.Names.TypeName.to_string t)
            | `Value (p, t) -> Format.fprintf ppf "%a.%s" resolved_path (cast p) (Odoc_model.Names.ValueName.to_string t)
            | `Constructor (p, t) -> Format.fprintf ppf "%a.%s" resolved_path (cast p) (Odoc_model.Names.ConstructorName.to_string t)
            | `OpaqueModule m -> Format.fprintf ppf "opaquemodule(%a)" resolved_path (cast m)
            | `OpaqueModuleType m -> Format.fprintf ppf "opaquemoduletype(%a)" resolved_path (cast m)
            | `SubstT (_, _)
            | `CanonicalModuleType (_, _)
            | `CanonicalType (_, _)
            | `CanonicalDataType (_, _)
            | `Class (_, _)
            | `ClassType (_, _)
            | `Hidden _
            | `Canonical _ -> Format.fprintf ppf "unimplemented resolved_path"

        and path : Format.formatter -> Odoc_model.Paths.Path.t -> unit =
            fun ppf (p : Odoc_model.Paths.Path.t) ->
            match p with
            | `Resolved rp -> Format.fprintf ppf "resolved[%a]" resolved_path (rp :> Odoc_model.Paths.Path.Resolved.t)
            | `Identifier (i,b) -> Format.fprintf ppf "identifier(%a,%b)" identifier i b
            | `Root s -> Format.fprintf ppf "%s" s
            | `Forward s -> Format.fprintf ppf "%s" s
            | `Dot (parent,s) -> Format.fprintf ppf "%a.%s" path (parent :> Odoc_model.Paths.Path.t) s
            | `Apply (func,arg) -> Format.fprintf ppf "%a(%a)" path (func :> Odoc_model.Paths.Path.t) path (arg :> Odoc_model.Paths.Path.t)

        and model_fragment ppf (f : Odoc_model.Paths.Fragment.t) =
            match f with
            | `Root -> ()
            | `Resolved rf -> model_resolved_fragment ppf rf
            | `Dot (sg, d) -> Format.fprintf ppf "*%a.%s" model_fragment (sg :> Odoc_model.Paths.Fragment.t) d

        and model_resolved_fragment ppf (f : Odoc_model.Paths.Fragment.Resolved.t) =
            match f with
            | `Root (`Module p) -> Format.fprintf ppf "root_module(%a)" resolved_path (p :> Odoc_model.Paths.Path.Resolved.t) 
            | `Root (`ModuleType p) -> Format.fprintf ppf "root_module_type(%a)" resolved_path (p :> Odoc_model.Paths.Path.Resolved.t) 
            | `Module (sg, m) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> Odoc_model.Paths.Fragment.Resolved.t) (Odoc_model.Names.ModuleName.to_string m)
            | `Type (sg, m) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> Odoc_model.Paths.Fragment.Resolved.t) (Odoc_model.Names.TypeName.to_string m)
            | _ -> Format.fprintf ppf "UNIMPLEMENTED model_resolved_fragment"

    end

end

let my_compilation_unit id (s : Odoc_model.Lang.Signature.t) =
    { Odoc_model.Lang.Compilation_unit.
      id = id
    ; root = root
    ; digest = "nodigest"
    ; imports = []
    ; source = None
    ; interface = true
    ; hidden = false
    ; content = Module s
    ; expansion = None
    ; linked = false
    ; canonical = None
    ; source_info = None
    ; shape_info = None
}

let mkresolver () =
  Odoc_odoc.Resolver.create
    ~important_digests:false
    ~directories:(List.map Odoc_odoc.Fs.Directory.of_string
#if OCAML_VERSION >= (5,2,0)
  (let paths = Load_path.get_paths () in
   List.filter (fun s -> s <> "") (paths.visible @ paths.hidden))
#elif OCAML_VERSION >= (4,8,0)
    (Load_path.get_paths () |> List.filter (fun s -> s <> ""))
#else
    !Config.load_path
#endif
    ) ~open_modules:[]

let warnings_options =
  { Odoc_model.Error.warn_error = false; print_warnings = true }

let handle_warnings ww =
  match Odoc_model.Error.handle_warnings ~warnings_options ww with
  | Ok x -> x
  | Error (`Msg msg) -> failwith msg

let resolve unit =
  let resolver = mkresolver () in
  let resolve_env = Odoc_odoc.Resolver.build_compile_env_for_unit resolver unit in
  Odoc_xref2.Compile.compile ~filename:"<test>" resolve_env unit
  |> handle_warnings

let resolve_from_string s =
  let id, sg, _ = model_of_string s in
  let unit = my_compilation_unit id sg in
  resolve unit

let compile_signature ?(id = id) sg =
  let open Odoc_xref2 in
  Lookup_failures.catch_failures ~filename:"<test>" (fun () ->
      Compile.signature Env.empty id sg)
  |> handle_warnings

let compile_mli test_data =
  let sg = signature_of_mli_string test_data in
  compile_signature sg
