# Testing {!modules:...} lists

  $ compile external.mli main.mli

Everything should resolve:

  $ odoc_print main.odocl | jq -c '.. | .["`Modules"]? | select(.) | .[] | .[]'
  {"`Resolved":{"`Identifier":{"`Root":[{"`RootPage":"test"},"External"]}}}
  "None"
  {"`Resolved":{"`Module":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"External"]}},"X"]}}
  {"Some":[{"`Word":"Doc"},"`Space",{"`Word":"for"},"`Space",{"`Code_span":"X"},{"`Word":"."}]}
  {"`Resolved":{"`Identifier":{"`Root":[{"`RootPage":"test"},"Main"]}}}
  "None"
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Internal"]}}}
  {"Some":[{"`Word":"Doc"},"`Space",{"`Word":"for"},"`Space",{"`Code_span":"Internal"},{"`Word":"."}]}
  {"`Resolved":{"`Module":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Internal"]}},"Y"]}}
  {"Some":[{"`Word":"Doc"},"`Space",{"`Word":"for"},"`Space",{"`Word":"Internal."},{"`Code_span":"X"},{"`Word":"."},"`Space",{"`Word":"An"},"`Space",{"`Word":"other"},"`Space",{"`Word":"sentence."}]}
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Z"]}}}
  "None"
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"F"]}}}
  "None"
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Type_of"]}}}
  "None"
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Type_of_str"]}}}
  "None"
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"With_type"]}}}
  "None"
  {"`Resolved":{"`SubstAlias":[{"`Module":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"External"]}},"X"]},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Alias"]}}]}}
  "None"
  {"`Resolved":{"`SubstAlias":[{"`Canonical":[{"`Module":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Internal"]}},"C1"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"C1"]}}}]},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"C1"]}}]}}
  "None"
  {"`Resolved":{"`SubstAlias":[{"`Canonical":[{"`Module":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Internal"]}},"C2"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"C2"]}}}]},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"C2"]}}]}}
  "None"
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Inline_include"]}}}
  "None"

'Type_of' and 'Alias' don't have a summary. `C1` and `C2` neither, we expect at least `C2` to have one.
