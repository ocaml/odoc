  $ compile good_ref.mli

  $ compile bad_ref.mli

  $ jq_scan_references() { jq -c '.. | .["`Reference"]? | select(.) | .[0]'; }

  $ odoc_print good_ref.odocl | jq_scan_references
  {"`Resolved":{"`Identifier":{"`Value":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Good_ref"]},"(^)"]}}}
  {"`Resolved":{"`Identifier":{"`Value":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Good_ref"]},"(*)"]}}}

  $ odoc_print bad_ref.odocl | jq_scan_references
  {"`Resolved":{"`Identifier":{"`Value":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Bad_ref"]},"(^)"]}}}
