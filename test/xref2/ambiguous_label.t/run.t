Labels don't follow OCaml's scoping rules:
- No shadowing: There cannot be two identical labels.
- No nesting: It is not possible to disambiguate labels by nesting them inside sections.

  $ compile test.ml
  File "test.ml", line 25, characters 4-36:
  Failed to resolve reference unresolvedroot(example_2) Couldn't find "example_2"
  File "test.ml", line 16, characters 4-50:
  Reference to 'example' is ambiguous. Please specify its kind: section-example, section-example, section-example.

Contains some ambiguous labels:

  $ odoc_print test.odocl | jq -c '.. | .["`Heading"]? | select(.)'
  ["`Section",{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"section-1"]},[{"`Word":"Section"},"`Space",{"`Word":"1"}]]
  ["`Subsection",{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"example"]},[{"`Word":"Example"}]]
  ["`Section",{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"section-2"]},[{"`Word":"Section"},"`Space",{"`Word":"2"}]]
  ["`Subsection",{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"example"]},[{"`Word":"Example"}]]
  ["`Subsection",{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"example"]},[{"`Word":"Example"}]]
  ["`Subsection",{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"example_3"]},[{"`Word":"Example_3"}]]

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

  $ odoc_print test.odocl | jq -c '.. | .["`Reference"]? | select(.)'
  [{"`Resolved":{"`Identifier":{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"example"]}}},[{"`Word":"Should"},"`Space",{"`Word":"resolve"},"`Space",{"`Word":"to"},"`Space",{"`Word":"the"},"`Space",{"`Word":"first"},"`Space",{"`Word":"label"}]]
  [{"`Root":["example_2","`TUnknown"]},[{"`Word":"Shouldn't"},"`Space",{"`Word":"resolve"}]]
