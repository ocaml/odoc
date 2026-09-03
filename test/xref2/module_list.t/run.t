# Testing {!modules:...} lists

  $ compile external.mli starts_with_open.mli main.mli
  File "main.mli", line 63, characters 22-43:
  Warning: Failed to resolve reference unresolvedroot(Resolve_synopsis).t Couldn't find "Resolve_synopsis"
  File "main.mli", line 63, characters 17-21:
  Warning: Failed to resolve reference unresolvedroot(t) Couldn't find "t"
  File "external.mli", line 9, characters 6-10:
  Warning: Failed to resolve reference unresolvedroot(t) Couldn't find "t"
  File "main.mli", line 63, characters 22-43:
  Warning: Failed to resolve reference unresolvedroot(Resolve_synopsis).t Couldn't find "Resolve_synopsis"
  File "main.mli", line 63, characters 17-21:
  Warning: Failed to resolve reference unresolvedroot(t) Couldn't find "t"

Everything should resolve:

  $ odoc_print main.odocl | jq -c '.. | .["`Modules"]? | select(.) | .[] | .[]'
  {"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"External"]}}}
  {"Some":[{"`Word":"Doc for "},{"`Code_span":"External"},{"`Word":"."}]}
  {"`Resolved":{"`Module":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"External"]}},"X"]}}
  {"Some":[{"`Word":"Doc for "},{"`Code_span":"X"},{"`Word":"."}]}
  {"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]}}}
  "None"
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"Internal"]}}}
  {"Some":[{"`Word":"Doc for "},{"`Code_span":"Internal"},{"`Word":"."}]}
  {"`Resolved":{"`Module":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"Internal"]}},"Y"]}}
  {"Some":[{"`Word":"Doc for Internal."},{"`Code_span":"X"},{"`Word":". An other sentence."}]}
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"Z"]}}}
  {"Some":[{"`Word":"Doc for "},{"`Code_span":"Z"},{"`Word":"."}]}
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"F"]}}}
  {"Some":[{"`Word":"Doc for "},{"`Code_span":"F ()"},{"`Word":"."}]}
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"Type_of"]}}}
  "None"
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"Type_of_str"]}}}
  {"Some":[{"`Word":"Doc of "},{"`Code_span":"Type_of_str"},{"`Word":"."}]}
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"With_type"]}}}
  {"Some":[{"`Word":"Doc for "},{"`Code_span":"T"},{"`Word":"."}]}
  {"`Resolved":{"`Alias":[{"`Module":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"External"]}},"X"]},{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"Alias"]}}]}}
  {"Some":[{"`Word":"Doc for "},{"`Code_span":"X"},{"`Word":"."}]}
  {"`Resolved":{"`Alias":[{"`Canonical":[{"`Module":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"Internal"]}},"C1"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"C1"]}}}]},{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"C1"]}}]}}
  {"Some":[{"`Word":"Doc for "},{"`Code_span":"C1"},{"`Word":"."}]}
  {"`Resolved":{"`Alias":[{"`Canonical":[{"`Module":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"Internal"]}},"C2"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"C2"]}}}]},{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"C2"]}}]}}
  "None"
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"Inline_include"]}}}
  {"Some":[{"`Word":"Doc for "},{"`Code_span":"T"},{"`Word":"."}]}
  {"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Starts_with_open"]}}}
  {"Some":[{"`Word":"Synopsis of "},{"`Code_span":"Starts_with_open"},{"`Word":"."}]}
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]},"Resolve_synopsis"]}}}
  {"Some":[{"`Word":"This should be resolved when included: "},{"`Reference":[{"`Resolved":{"`Type":[{"`Module":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]}},"Resolve_synopsis"]},"t"]}},[]]},{"`Word":". These\n      shouldn't: "},{"`Reference":[{"`Root":["t","`TUnknown"]},[]]},{"`Space":" "},{"`Reference":[{"`Dot":[{"`Root":["Resolve_synopsis","`TUnknown"]},"t"]},[]]}]}
  {"`Resolved":{"`Module":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"External"]}},"Resolve_synopsis"]}}
  {"Some":[{"`Reference":[{"`Root":["t","`TUnknown"]},[]]}]}

References in the synopses above should be resolved.
'External' contains a module list too:

  $ odoc_print external.odocl | jq -c '.. | .["`Modules"]? | select(.) | .[] | .[]'
  {"`Resolved":{"`Module":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]}},"Resolve_synopsis"]}}
  {"Some":[{"`Word":"This should be resolved when included: "},{"`Reference":[{"`Resolved":{"`Type":[{"`Module":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Main"]}},"Resolve_synopsis"]},"t"]}},[]]},{"`Word":". These\n      shouldn't: "},{"`Reference":[{"`Root":["t","`TUnknown"]},[]]},{"`Space":" "},{"`Reference":[{"`Dot":[{"`Root":["Resolve_synopsis","`TUnknown"]},"t"]},[]]}]}

'Type_of' and 'Alias' don't have a summary. `C1` and `C2` neither, we expect at least `C2` to have one.
