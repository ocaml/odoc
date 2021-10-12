# References to pages and items in pages

  $ compile p.mld good_references.mli bad_references.mli
  File "good_references.mli", line 8, characters 33-40:
  Failed to resolve reference unresolvedroot(p).P1 Couldn't find "p"
  File "good_references.mli", line 8, characters 28-32:
  Failed to resolve reference unresolvedroot(p) Couldn't find "p"
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
  {"`Root":["p","`TUnknown"]}
  {"`Dot":[{"`Root":["p","`TUnknown"]},"P1"]}

Every references in `Bad_references` should not:

  $ odoc_print bad_references.odocl | jq_scan_references
  {"`Root":["not_found","`TPage"]}
  {"`Label":[{"`Root":["p","`TPage"]},"not_found"]}

Every references in `P` should resolve:

  $ odoc_print page-p.odocl | jq_scan_references
  {"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Good_references"]}}}
  {"`Resolved":{"`Type":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Good_references"]}},"t"]}}
