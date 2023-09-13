Labels don't follow OCaml's scoping rules:
- No shadowing: There cannot be two identical labels.
- No nesting: It is not possible to disambiguate labels by nesting them inside sections.

  $ compile test.ml test_2.ml
  File "test.ml", line 25, characters 4-36:
  Warning: Failed to resolve reference unresolvedroot(example_2) Couldn't find "example_2"
  File "test.ml", line 16, characters 4-50:
  Warning: Multiple sections named 'example' found. Please alter one to ensure reference is unambiguous. Locations:
    File "test.ml", line 3, character 4
    File "test.ml", line 18, character 4
    File "test.ml", line 9, character 4

Contains some ambiguous labels:

  $ odoc_print test.odocl | jq -c '.. | .["`Heading"]? | select(.) | .[1]'
  {"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"section-1"]}
  {"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"example"]}
  {"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"section-2"]}
  {"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"example"]}
  {"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"example"]}
  {"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"example_3"]}

  $ odoc html-generate --indent -o html test.odocl

The table of content should point to unique anchors:

  $ sed -n '/<nav class="odoc-toc">$/,/<\/nav>/p' html/test/Test/index.html
    <nav class="odoc-toc">
     <ul>
      <li><a href="#section-1">Section 1</a>
       <ul><li><a href="#example">Example</a></li></ul>
      </li>
      <li><a href="#section-2">Section 2</a>
       <ul><li><a href="#example_2">Example</a></li>
        <li><a href="#example__3">Example</a></li>
        <li><a href="#example_3">Example_3</a></li>
       </ul>
      </li>
     </ul>
    </nav>

References should resolve to the first occurence of the ambiguous label. It is
not possible to use the internal label name in references:

  $ odoc_print test.odocl | jq -c '.. | .["`Reference"]? | select(.)'
  [{"`Resolved":{"`Identifier":{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"example"]}}},[{"`Word":"Should"},"`Space",{"`Word":"resolve"},"`Space",{"`Word":"to"},"`Space",{"`Word":"the"},"`Space",{"`Word":"first"},"`Space",{"`Word":"label"}]]
  [{"`Root":["example_2","`TUnknown"]},[{"`Word":"Shouldn't"},"`Space",{"`Word":"resolve"}]]

A second module has a reference to the ambiguous label:

  $ odoc_print test_2.odocl | jq -c '.. | .["`Reference"]? | select(.)'
  [{"`Resolved":{"`Label":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]}},"example"]}},[{"`Word":"Should"},"`Space",{"`Word":"resolve"},"`Space",{"`Word":"to"},"`Space",{"`Word":"the"},"`Space",{"`Word":"first"},"`Space",{"`Word":"label"}]]
