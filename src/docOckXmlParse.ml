
type 'a parser =
  { file : Xmlm.input -> 'a DocOckTypes.Unit.t;
    unit :  Xmlm.input -> 'a DocOckTypes.Unit.t }

type 'a token =
  { value: 'a;
    start: Lexing.position;
    finish: Lexing.position; }

let value x = x.value
let start x = x.start
let finish x = x.finish

let position (l, c) =
  Lexing.{ pos_fname = "";
           pos_lnum = l;
           pos_bol = -1;
           pos_cnum = c; }

let build (type base) (input_base : Xmlm.input -> base) =
  let module Parser = DocOckXmlParser.Make(struct type t = base end) in
  let open Parser in
  let plain_tags = Hashtbl.create 113 in
    Hashtbl.add plain_tags "alias" ALIAS;
    Hashtbl.add plain_tags "any" ANY;
    Hashtbl.add plain_tags "apply" APPLY;
    Hashtbl.add plain_tags "arguments" ARGUMENTS;
    Hashtbl.add plain_tags "arrow" ARROW;
    Hashtbl.add plain_tags "author" AUTHOR;
    Hashtbl.add plain_tags "before" BEFORE;
    Hashtbl.add plain_tags "bold" BOLD;
    Hashtbl.add plain_tags "center" CENTER;
    Hashtbl.add plain_tags "class" CLASS;
    Hashtbl.add plain_tags "class_type" CLASS_TYPE;
    Hashtbl.add plain_tags "closed" CLOSED;
    Hashtbl.add plain_tags "code" CODE;
    Hashtbl.add plain_tags "comment" COMMENT;
    Hashtbl.add plain_tags "constant" CONSTANT;
    Hashtbl.add plain_tags "constraint" CONSTRAINT;
    Hashtbl.add plain_tags "constructor" CONSTRUCTOR;
    Hashtbl.add plain_tags "deprecated" DEPRECATED;
    Hashtbl.add plain_tags "digest" DIGEST;
    Hashtbl.add plain_tags "doc" DOC;
    Hashtbl.add plain_tags "dot" DOT;
    Hashtbl.add plain_tags "element" ELEMENT;
    Hashtbl.add plain_tags "emphasize" EMPHASIZE;
    Hashtbl.add plain_tags "enum" ENUM;
    Hashtbl.add plain_tags "exception" EXCEPTION;
    Hashtbl.add plain_tags "extensible" EXTENSIBLE;
    Hashtbl.add plain_tags "extension" EXTENSION;
    Hashtbl.add plain_tags "external" EXTERNAL;
    Hashtbl.add plain_tags "field" FIELD;
    Hashtbl.add plain_tags "file" FILE;
    Hashtbl.add plain_tags "fixed" FIXED;
    Hashtbl.add plain_tags "functor" FUNCTOR;
    Hashtbl.add plain_tags "identifier" IDENTIFIER;
    Hashtbl.add plain_tags "import" IMPORT;
    Hashtbl.add plain_tags "include" INCLUDE;
    Hashtbl.add plain_tags "index" INDEX;
    Hashtbl.add plain_tags "inherit" INHERIT;
    Hashtbl.add plain_tags "instance_variable" INSTANCE_VARIABLE;
    Hashtbl.add plain_tags "italic" ITALIC;
    Hashtbl.add plain_tags "item" ITEM;
    Hashtbl.add plain_tags "label" LABEL;
    Hashtbl.add plain_tags "left" LEFT;
    Hashtbl.add plain_tags "link" LINK;
    Hashtbl.add plain_tags "list" LIST;
    Hashtbl.add plain_tags "method" METHOD;
    Hashtbl.add plain_tags "module" MODULE;
    Hashtbl.add plain_tags "modules" MODULES;
    Hashtbl.add plain_tags "module_subst" MODULE_SUBST;
    Hashtbl.add plain_tags "module_type" MODULE_TYPE;
    Hashtbl.add plain_tags "mutable" MUTABLE;
    Hashtbl.add plain_tags "name" NAME;
    Hashtbl.add plain_tags "neg" NEG;
    Hashtbl.add plain_tags "newline" NEWLINE;
    Hashtbl.add plain_tags "object" OBJECT;
    Hashtbl.add plain_tags "open" OPEN;
    Hashtbl.add plain_tags "optional" OPTIONAL;
    Hashtbl.add plain_tags "package" PACKAGE;
    Hashtbl.add plain_tags "param" PARAM;
    Hashtbl.add plain_tags "path" PATH;
    Hashtbl.add plain_tags "poly" POLY;
    Hashtbl.add plain_tags "poly_variant" POLY_VARIANT;
    Hashtbl.add plain_tags "pos" POS;
    Hashtbl.add plain_tags "precode" PRECODE;
    Hashtbl.add plain_tags "primitive" PRIMITIVE;
    Hashtbl.add plain_tags "private" PRIVATE;
    Hashtbl.add plain_tags "raise" RAISE;
    Hashtbl.add plain_tags "record" RECORD;
    Hashtbl.add plain_tags "reference" REFERENCE;
    Hashtbl.add plain_tags "resolved" RESOLVED;
    Hashtbl.add plain_tags "result" RESULT;
    Hashtbl.add plain_tags "return" RETURN;
    Hashtbl.add plain_tags "right" RIGHT;
    Hashtbl.add plain_tags "root" ROOT;
    Hashtbl.add plain_tags "section" SECTION;
    Hashtbl.add plain_tags "see" SEE;
    Hashtbl.add plain_tags "signature" SIGNATURE;
    Hashtbl.add plain_tags "since" SINCE;
    Hashtbl.add plain_tags "special" SPECIAL;
    Hashtbl.add plain_tags "stop" STOP;
    Hashtbl.add plain_tags "subscript" SUBSCRIPT;
    Hashtbl.add plain_tags "superscript" SUPERSCRIPT;
    Hashtbl.add plain_tags "tag" TAG;
    Hashtbl.add plain_tags "tuple" TUPLE;
    Hashtbl.add plain_tags "type" TYPE;
    Hashtbl.add plain_tags "typeof" TYPEOF;
    Hashtbl.add plain_tags "type_subst" TYPE_SUBST;
    Hashtbl.add plain_tags "unit" UNIT;
    Hashtbl.add plain_tags "url" URL;
    Hashtbl.add plain_tags "value" VALUE;
    Hashtbl.add plain_tags "var" VAR;
    Hashtbl.add plain_tags "variant" VARIANT;
    Hashtbl.add plain_tags "verbatim" VERBATIM;
    Hashtbl.add plain_tags "version" VERSION;
    Hashtbl.add plain_tags "virtual" VIRTUAL;
    Hashtbl.add plain_tags "with" WITH;
    let lex input () =
      if Xmlm.eoi input then
        { value = EOF;
          start = position (Xmlm.pos input);
          finish = position (Xmlm.pos input) }
      else
        let start = position (Xmlm.pos input) in
        let token = Xmlm.input input in
        let finish = position (Xmlm.pos input) in
        let value =
          match token with
          | `Dtd _ -> DTD
          | `Data s -> Data s
          | `El_start ((namespace, tag), attrs) ->
              if namespace <> "" then raise Error
              else begin
                try
                  Hashtbl.find plain_tags tag
                with Not_found ->
                  match tag with
                  | "argument" ->
                      let pos =
                        try
                          let pos = List.assoc ("", "pos") attrs in
                            Some (int_of_string pos)
                        with Not_found | Failure _ -> None
                      in
                        Argument pos
                  | "custom" ->
                      let tag =
                        try
                          List.assoc ("", "tag") attrs
                        with Not_found -> raise Error
                      in
                        Custom tag
                  | "target" ->
                      let name =
                        try
                          Some (List.assoc ("", "name") attrs)
                        with Not_found -> None
                      in
                        Target name
                  | "title" ->
                      let level =
                        try
                          int_of_string (List.assoc ("", "level") attrs)
                        with Not_found | Failure _ -> raise Error
                      in
                        Title level
                  | "base" ->
                      let base = input_base input in
                        if Xmlm.eoi input then raise Error;
                        let token = Xmlm.input input in
                          if token <> `El_end then raise Error;
                          Base base
                  | _ -> raise Error
                end
          | `El_end -> CLOSE
        in
          {value; start; finish}
    in
    let filep =
      MenhirLib.Convert.traditional2revised value start finish Parser.file
    in
    let unitp =
      MenhirLib.Convert.traditional2revised value start finish Parser.unit
    in
    let file input = filep (lex input) in
    let unit input = unitp (lex input) in
      {file; unit}

let file parser = parser.file

let unit parser = parser.unit
