# References to pages and items in pages

  $ compile p.mld good_references.mli bad_references.mli

Every references in `Good_references` should resolve:

  $ jq_scan_references() { jq -c '.. | .["`Reference"]? | select(.) | .[0]'; }

  $ odoc_print good_references.odocl | jq_scan_references
  {"`Resolved":{"`Identifier":{"`LeafPage":[{"`RootPage":"test"},"p"]}}}
  {"`Resolved":{"`Identifier":{"`Label":[{"`LeafPage":[{"`RootPage":"test"},"p"]},"P1"]}}}
  {"`Resolved":{"`Identifier":{"`Label":[{"`LeafPage":[{"`RootPage":"test"},"p"]},"P2"]}}}
  {"`Resolved":{"`Identifier":{"`Label":[{"`LeafPage":[{"`RootPage":"test"},"p"]},"P1"]}}}

Every references in `Bad_references` should not:

  $ odoc_print bad_references.odocl | jq_scan_references
  {"`Root":["not_found","`TPage"]}
  {"`Label":[{"`Root":["p","`TPage"]},"not_found"]}

Every references in `P` should resolve:

  $ odoc_print page-p.odocl | jq_scan_references
  {"`Root":["M","`TUnknown"]}
  {"`Dot":[{"`Root":["M","`TUnknown"]},"t"]}
