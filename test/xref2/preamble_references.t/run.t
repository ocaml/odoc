This is a test for the issue https://github.com/ocaml/odoc/issues/713

  $ compile m.mli

  $ jq_scan_references() { jq -c '.. | .["`Reference"]? | select(.) | .[0]'; }
  $ odoc_print m.odocl | jq_scan_references
  {"`Resolved":{"`Identifier":{"`Type":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"M"]},"M"]},"t"]}}}
  {"`Resolved":{"`Identifier":{"`Type":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"M"]},"M2"]},"t"]}}}
