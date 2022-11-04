# Testing the scope of references

  $ compile a.mli shadowed.mli shadowed_through_open.mli
  File "a.mli", line 18, characters 6-24:
  Warning: Failed to resolve reference unresolvedroot(C) Couldn't find "C"

  $ jq_scan_references() { jq -c '.. | .["`Reference"]? | select(.)'; }

The references from a.mli, see the attached text to recognize them:

  $ odoc_print a.odocl | jq_scan_references
  [{"`Resolved":{"`Module":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"A"]},"B"]}},"C"]}},[{"`Word":"Defined-below"}]]
  [{"`Resolved":{"`Module":[{"`Module":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"A"]}},"B"]},"C"]}},[{"`Word":"Defined-below-but-absolute"}]]
  [{"`Root":["C","`TUnknown"]},[{"`Word":"Through-open"}]]
  [{"`Resolved":{"`Module":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"A"]},"B"]}},"C"]}},[{"`Word":"Doc-relative"}]]
  [{"`Resolved":{"`Module":[{"`Module":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"A"]}},"B"]},"C"]}},[{"`Word":"Doc-absolute"}]]

References should be resolved after the whole signature has been added to the
scope. Both "Before-shadowed" and "After-shadowed" should resolve to [M.t].

  $ odoc_print shadowed.odocl | jq_scan_references
  [{"`Resolved":{"`Identifier":{"`Type":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Shadowed"]},"M"]},"t"]}}},[{"`Word":"Before-shadowed"}]]
  [{"`Resolved":{"`Type":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Shadowed"]}},"t"]}},[]]
  [{"`Resolved":{"`Identifier":{"`Type":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Shadowed"]},"M"]},"t"]}}},[{"`Word":"After-shadowed"}]]
  [{"`Resolved":{"`Identifier":{"`Value":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Shadowed"]},"N"]},"f"]}}},[]]
  [{"`Resolved":{"`Value":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Shadowed"]}},"f"]}},[]]

"Before-open" and "After-open" should resolve to to [T.t].
"Before-include" and "After-include" should resolve to [Through_include.t].

  $ odoc_print shadowed_through_open.odocl | jq_scan_references
  [{"`Resolved":{"`Identifier":{"`Type":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Shadowed_through_open"]},"t"]}}},[{"`Word":"Before-open"}]]
  [{"`Resolved":{"`Identifier":{"`Type":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Shadowed_through_open"]},"t"]}}},[{"`Word":"After-open"}]]
  [{"`Resolved":{"`Identifier":{"`Type":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Shadowed_through_open"]},"Through_include"]},"t"]}}},[{"`Word":"Before-include"}]]
  [{"`Resolved":{"`Identifier":{"`Type":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Shadowed_through_open"]},"Through_include"]},"t"]}}},[{"`Word":"After-include"}]]
