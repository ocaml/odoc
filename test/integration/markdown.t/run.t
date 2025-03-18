  $ ocamlc -c -bin-annot test.mli
  $ ocamlc -c -bin-annot test2.mli
  $ printf "{0 The title}\n something else" > page.mld
  $ odoc compile --package test test.cmti
  File "test.mli", line 1, characters 4-12:
  Warning: '{0': heading level should be lower than top heading level '0'.
  $ odoc compile --package test -I . test2.cmti
  $ odoc compile --package test -I . page.mld
  $ odoc link test.odoc
  File "test.mli", line 304, characters 32-75:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).tags Couldn't find page "odoc_for_authors"
  File "test.mli", line 293, characters 12-47:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).tags Couldn't find page "odoc_for_authors"
  File "test.mli", line 230, characters 12-45:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).tables Couldn't find "odoc_for_authors"
  File "test.mli", line 224, characters 10-43:
  Warning: Failed to resolve reference ./odoc_logo_placeholder.jpg Path 'odoc_logo_placeholder.jpg' not found
  File "test.mli", line 215, characters 12-45:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).media Couldn't find "odoc_for_authors"
  File "test.mli", line 196, characters 12-42:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).math Couldn't find "odoc_for_authors"
  File "test.mli", line 187, characters 12-57:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).verbatim_blocks Couldn't find "odoc_for_authors"
  File "test.mli", line 152, characters 12-56:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).code_blocks Couldn't find "odoc_for_authors"
  File "test.mli", line 115, characters 12-44:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).lists Couldn't find "odoc_for_authors"
  File "test.mli", line 110, characters 14-68:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).links_and_references Couldn't find "odoc_for_authors"
  File "test.mli", line 108, characters 14-67:
  Warning: Failed to resolve reference /cmdliner/tutorial Path '/cmdliner/tutorial' not found
  File "test.mli", line 106, characters 14-64:
  Warning: Failed to resolve reference /fmt/Fmt.pf Path '/fmt/Fmt' not found
  File "test.mli", line 104, characters 14-57:
  Warning: Failed to resolve reference unresolvedroot(Odoc_odoc).Compile.compile Couldn't find "Odoc_odoc"
  File "test.mli", line 102, characters 14-42:
  Warning: Failed to resolve reference unresolvedroot(Odoc_odoc).Compile.compile Couldn't find "Odoc_odoc"
  File "test.mli", line 87, characters 12-64:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).links_and_references Couldn't find "odoc_for_authors"
  File "test.mli", line 72, characters 12-58:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).links_and_references Couldn't find "odoc_for_authors"
  File "test.mli", line 63, characters 12-66:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).basics Couldn't find "odoc_for_authors"
  File "test.mli", line 54, characters 12-65:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).basics Couldn't find "odoc_for_authors"
  File "test.mli", line 27, characters 12-50:
  Warning: Failed to resolve reference unresolvedroot(odoc_for_authors).sections Couldn't find "odoc_for_authors"
  $ odoc link test2.odoc
  $ odoc link page-page.odoc
  $ odoc markdown-generate test.odocl -o markdown
  # Test
  ## Test
  Quick reference for the odoc language\!
  \|  \| odoc   syntax \| Render   as \|
  \| --- \| --- \| --- \|
  \| Paragraphs \|   A first paragraph&#10;&#10;  A second paragraph \| A   first   paragraph A   second   paragraph \|
  \| Headings \|   {1 Title}&#10;  {2 Subtitle}&#10;  {3 Subsubtitle}&#10;&#10;  {3:my\_id Referenceable title}&#10;&#10;  See {!my\_id}. Standalone   pages   must   start   with   a   0   heading:   {0 Page big title} \|       See   \|
  \| Bold,   italic   and   emphasis \|   {b bold} text, {i italic} text, {e emphasized} text \| bold   text,   italic   text,   emphasized   text \|
  \| Subscripts   and   superscript \|   H{\_ 2}O and 1{^ st} \| H 2 O   and   1 st \|
  \| Link \|   Here is a link: {:https://www.example.com}.&#10;&#10;  You can also click {{:https://www.example.com}here}. \| Here   is   a   link:   https://www.example.com . You   can   also   click   here . \|
  \| References \|   See {!Odoc\_odoc.Compile.compile}.&#10;&#10;  See {{!Odoc\_odoc.Compile.compile}this function}.&#10;&#10;  See {{!/fmt/Fmt.pf}this function from another library}.&#10;&#10;  See {{!/cmdliner/tutorial}this page from another package}.&#10;&#10;  See {{!odoc\_for\_authors.links\_and\_references}this section} for the syntax of references. \| See   Odoc\_odoc.Compile.compile . See   this   function . See   this   function   from   another   library . See   this   page   from   another   package . See   this   section   for   the   syntax   of   references. \|
  \| Lists \|   - First item&#10;  - Second item&#10;&#10;  + First ordered item&#10;  + Second numbered item&#10;&#10;  {ul&#10;    {- First item}&#10;    {- Second item}&#10;    {li can also be used}}&#10;&#10;  {ol&#10;    {- First numbered item}&#10;    {- Second numbered item}&#10;    {li can also be used}} \| First   item Second   item First   ordered   item Second   numbered   item First   item Second   item can   also   be   used First   numbered   item Second   numbered   item can   also   be   used \|
  \| Code   Blocks \|   Inline \[code\].&#10;&#10;  {\[&#10;  let \_ = "Block code"&#10;  \]}&#10;&#10;  {foo@text\[&#10;  Code block with {\[inner code block syntax\]}&#10;  \]foo}&#10;&#10;  {@python\[&#10;  \[i+1 for i in xrange(2)\]&#10;  \]} \| Inline   code .   let \_ = "Block code"   Code block with {\[inner code block syntax\]}   \[i+1 for i in xrange(2)\] \|
  \| Verbatim \|   {v verbatim text v} \| verbatim text \|
  \| Math \|   For inline math: {m \\sqrt 2}.&#10;&#10;  For display math:&#10;&#10;  {math \\sqrt 2} \| For   inline   math:   . For   display   math: \|
  \| Images \|   {image!path/to/file.png}&#10;&#10;  {image:https://picsum.photos/200/100} \|  \|
  \| Table \|   Light syntax:&#10;&#10;  {t \| Header 1 \| Header 2 \|&#10;     \|----------\|----------\|&#10;     \| Cell 1   \| Cell 2   \|&#10;     \| Cell 3   \| Cell 4   \|}&#10;&#10;  Explicit syntax:&#10;&#10;  {table&#10;    {tr&#10;      {th Header 1}&#10;      {th Header 2}}&#10;    {tr&#10;      {td Cell 1}&#10;      {td Cell 2}}&#10;    {tr&#10;      {td Cell 3}&#10;      {td Cell 4}}} \| Light   syntax: Explicit   syntax: \|
  \| HTML \|   {%html:&#10;    \<blockquote\>&#10;      Odoc language lack support for quotation!&#10;    \</blockquote\>&#10;  %} \|  \|
  \| Tags \|   @since 4.08&#10;&#10;  Tags are explained in {{!page-odoc\_for\_authors.tags}this section}. \| Since   4.08. Tags   are   explained   in   this   section . \|
  $ cat markdown/test/Test.html
