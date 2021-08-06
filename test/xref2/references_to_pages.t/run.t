# References to pages and items in pages

  $ compile p.mld good_references.mli bad_references.mli
  File "p.mld", line 6, characters 5-11:
  Failed to resolve reference unresolvedroot(M).t Couldn't find "M"
  File "p.mld", line 6, characters 0-4:
  Failed to resolve reference unresolvedroot(M) Couldn't find "M"
  File "bad_references.mli", line 6, characters 42-69:
  Failed to resolve reference unresolvedroot(p).not_found Couldn't find page "not_found"
  File "bad_references.mli", line 4, characters 20-37:
  Failed to resolve reference unresolvedroot(not_found) Couldn't find page "not_found"

Every references in `Good_references` should resolve:

  $ jq_scan_references() { jq -c '.. | .["`Reference"]? | select(.) | .[0]'; }

  $ odoc_print good_references.odocl | jq_scan_references
  {"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","test"]}},"p"]}}}
  {"`Resolved":{"`Identifier":{"`Label":[{"`LeafPage":[{"Some":{"`Page":["None","test"]}},"p"]},"P1"]}}}
  {"`Resolved":{"`Identifier":{"`Label":[{"`LeafPage":[{"Some":{"`Page":["None","test"]}},"p"]},"P2"]}}}
  {"`Resolved":{"`Identifier":{"`Label":[{"`LeafPage":[{"Some":{"`Page":["None","test"]}},"p"]},"P1"]}}}

Every references in `Bad_references` should not:

  $ odoc_print bad_references.odocl | jq_scan_references
  {"`Root":["not_found","`TPage"]}
  {"`Label":[{"`Root":["p","`TPage"]},"not_found"]}

Every references in `P` should resolve:

  $ odoc_print page-p.odocl | jq_scan_references
  {"`Root":["M","`TUnknown"]}
  {"`Dot":[{"`Root":["M","`TUnknown"]},"t"]}
