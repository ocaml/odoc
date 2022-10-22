open Tyxml.Html

let list_of_option = function
  | None -> []
  | Some x -> [ x ]

let render_result r =
  let open Db.Types.Elt in
  div
    ~a:[ a_class [ "pkg" ] ]
    [ a
        ~a:[ a_href (pkg_link r) ]
        [ txt (fst r.pkg)
        ; txt " "
        ; span ~a:[ a_class [ "version" ] ] [ txt (snd r.pkg) ]
        ]
    ]
  :: pre
       [ txt "val "
       ; a ~a:[ a_href (link r) ] [ em [ txt r.name ] ]
       ; txt " : "
       ; txt r.str_type
       ]
  :: list_of_option r.doc

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
    ]
