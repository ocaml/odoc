open Tyxml.Html

let list_of_option = function
  | None -> []
  | Some x -> [ x ]

let render_link elt =
  let open Db.Elt in
  match link elt with
  | Some link -> [ a_href link ]
  | None -> []

let render_elt elt =
  let open Db.Elt in
  let link = render_link elt in
  match elt.kind with
  | Val { type_; _ } ->
      [ txt "val "
      ; a ~a:link [ em [ txt elt.name ] ]
      ; txt " : "
      ; txt type_.txt
      ]
  | Doc -> [ txt "comment "; a ~a:link [ em [ txt elt.name ] ] ]
  | TypeDecl { html = type_decl } ->
      [ txt "type "
      ; a ~a:link [ em [ txt elt.name ] ]
      ; txt " = "
      ; Unsafe.data type_decl
      ]
  | Module -> [ txt "module "; a ~a:link [ em [ txt elt.name ] ] ]
  | Exception -> [ txt "exception "; a ~a:link [ em [ txt elt.name ] ] ]
  | Class_type -> [ txt "class type "; a ~a:link [ em [ txt elt.name ] ] ]
  | Method -> [ txt "method "; a ~a:link [ em [ txt elt.name ] ] ]
  | Class -> [ txt "class "; a ~a:link [ em [ txt elt.name ] ] ]
  | TypeExtension ->
      [ txt "type extension "; a ~a:link [ em [ txt elt.name ] ] ]
  | ExtensionConstructor ->
      [ txt "ext constructor "; a ~a:link [ em [ txt elt.name ] ] ]
  | ModuleType -> [ txt "module type "; a ~a:link [ em [ txt elt.name ] ] ]
  | Constructor -> [ txt "constructor "; a ~a:link [ em [ txt elt.name ] ] ]
  | Field -> [ txt "field "; a ~a:link [ em [ txt elt.name ] ] ]
  | FunctorParameter ->
      [ txt "functor param "; a ~a:link [ em [ txt elt.name ] ] ]
  | ModuleSubstitution ->
      [ txt "module subst "; a ~a:link [ em [ txt elt.name ] ] ]
  | ModuleTypeSubstitution ->
      [ txt "module type subst "; a ~a:link [ em [ txt elt.name ] ] ]
  | InstanceVariable ->
      [ txt "instance variable "; a ~a:link [ em [ txt elt.name ] ] ]

let render_pkg elt =
  let open Db.Elt in
  match elt.pkg with
  | Some { name; version } ->
      let link = elt |> pkg_link |> Option.get in
      [ div
          ~a:[ a_class [ "pkg" ] ]
          [ a
              ~a:[ a_href link ]
              [ txt name
              ; txt " "
              ; span ~a:[ a_class [ "version" ] ] [ txt version ]
              ]
          ]
      ]
  | None -> []

let render_result elt =
  let open Db.Types.Elt in
  render_pkg elt @ [ pre (render_elt elt); Unsafe.data elt.doc.Db.Elt.html ]

let render ~pretty results =
  match results with
  | [] ->
      div ~a:[ a_class [ "query" ] ] [ txt "No results! "; code [ txt pretty ] ]
  | _ ->
      div
        [ div
            ~a:[ a_class [ "query" ] ]
            [ txt "Results for "; code [ txt pretty ] ]
        ; ul ~a:[ a_class [ "found" ] ]
          @@ List.map (fun r -> li (render_result r)) results
        ]

let ajax_reload =
  {js|
    var latest = 0;
    var current = 0;
    document.getElementById('q').addEventListener('input', function(e) {
      var param = encodeURIComponent(e.target.value);
      ++latest;
      var self = latest;
      var req = new XMLHttpRequest();
      req.onreadystatechange = function() {
        if (this.readyState === 4 && current < self) {
          current = self;
          document.getElementById('results').innerHTML = this.response;
        }
      };
      req.open('GET', '/api?q=' + param, true);
      req.send();
      var url = param === '' ? '/' : '/?q=' + param;
      history.replaceState(null, 'Sherlodoc', url);
    });
  |js}

let search_form query =
  div
    ~a:[ a_class [ "header" ] ]
    [ form
        ~a:[ a_method `Get ]
        [ input
            ~a:
              [ a_input_type `Text
              ; a_id "q"
              ; a_name "q"
              ; a_value query
              ; a_placeholder "Search..."
              ; a_autofocus ()
              ; a_autocomplete false
              ]
            ()
        ; input ~a:[ a_input_type `Submit; a_value "Search!" ] ()
        ]
    ; script (Unsafe.data ajax_reload)
    ]

let template query contents =
  html
    ~a:[ a_lang "en" ]
    (head
       (title (txt "Sherlodoc"))
       [ meta ~a:[ a_charset "UTF-8" ] ()
       ; meta
           ~a:
             [ a_name "viewport"
             ; a_content "width=device-width, initial-scale=1"
             ]
           ()
       ; link ~rel:[ `Stylesheet ] ~href:"/s.css" ()
       ])
  @@ body [ search_form query; div ~a:[ a_id "results" ] [ contents ] ]

let github_icon =
  let open Tyxml.Svg in
  Tyxml.Html.svg
    ~a:
      [ a_width (16., None)
      ; a_height (16.0, None)
      ; a_viewBox (0., 0., 16., 16.)
      ]
    [ path
        ~a:
          [ a_d
              "M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 \
               7.59.4.07.55-.17.55-.38 \
               0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 \
               1.08.58 1.23.82.72 1.21 1.87.87 \
               2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 \
               0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 \
               2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 \
               2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 \
               3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 \
               1.93-.01 2.2 0 .21.15.46.55.38A8.012 8.012 0 0 0 16 \
               8c0-4.42-3.58-8-8-8z"
          ]
        []
    ]

let link_to_repo =
  p
    ~a:[ a_class [ "ad" ] ]
    [ txt {|(* Read the source, fork and contribute to |}
    ; a
        ~a:[ a_href "https://github.com/art-w/sherlodoc" ]
        [ github_icon; txt "art-w/sherlodoc" ]
    ; txt " *)"
    ]

let link str = a ~a:[ a_href ("?q=" ^ Uri.pct_encode str) ] [ code [ txt str ] ]

let explain =
  div
    ~a:[ a_class [ "doc" ] ]
    [ h1 [ txt "Sherlodoc" ]
    ; p
        ~a:[ a_class [ "doc" ] ]
        [ txt
            "Fuzzy search in OCaml's documentation for almost all opam \
             packages."
        ]
    ; ul
        ~a:[ a_class [ "doc" ] ]
        [ li
            [ txt "Search by name: "
            ; link "concat map"
            ; txt " and "
            ; link "Lwt pool"
            ]
        ; li [ txt "Search by type with a colon: "; link ": list list -> list" ]
        ; li
            [ txt "Search on name and type with a colon separator: "
            ; link "Yojson : t -> string"
            ]
        ; li [ txt "Search for constructors of a type: "; link ": Gg.color" ]
        ; li
            [ txt "Use _ to omit a subtype and search for consumers of a type: "
            ; link ": Gg.color -> _"
            ]
        ; li
            [ txt "Products and reordering of arguments: "
            ; link ": 'a list -> ('a * int -> bool) -> 'a list"
            ]
        ]
    ; Packages.html
    ; link_to_repo
    ]
