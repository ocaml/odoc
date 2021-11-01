  $ compile good_ref.mli

  $ compile bad_ref.mli
  File "bad_ref.mli", line 7, characters 4-14:
  Failed to resolve reference unresolvedroot(( * )) Couldn't find "( * )"
  File "bad_ref.mli", line 3, characters 4-12:
  Failed to resolve reference unresolvedroot(( ^ )) Couldn't find "( ^ )"

  $ jq_scan_references() { jq -c '.. | .["`Reference"]? | select(.) | .[0]'; }

  $ odoc_print good_ref.odocl | jq_scan_references
  {"`Resolved":{"`Identifier":{"`Value":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Good_ref"]},"(^)"]}}}
  {"`Resolved":{"`Identifier":{"`Value":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Good_ref"]},"(*)"]}}}

  $ odoc_print bad_ref.odocl | jq_scan_references
  {"`Root":["( ^ )","`TUnknown"]}
  {"`Root":["( * )","`TUnknown"]}
