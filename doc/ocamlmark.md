`ocamlmark` – An `ocamldoc` to CommonMark bi-directional translation
====================================================================

The `ocamlmark` language allows to write OCaml documentation strings
and `.mld` files in [CommonMark]. 

It is just the CommonMark language with the following tweaks:

1. Link references `[label]` whose `label` starts with `!` cannot be
   defined in documents and are used to stand for the `{!…}`,
   `{!modules…}` and `{!indexlist}` references and directives 
   of `ocamldoc`.
2. An extension to specify heading reference labels to match the
   `{[0-5]+:label text}` `ocamldoc` heading construct.  We reuse
   [Pandoc's heading identifiers][pandoc-heading-ids]'s syntax.   
3. In paragraphs, a few more construct are detected to support
   `ocamldoc`'s [`@-tags`][ocamldoc-@tag].

See the [full translation](#translation).

## Short example

The following show equivalent `ocamldoc` and `ocamlmark` text side-by-side:

```
{1:mapping Mapping list elements}   | # Mapping list elements {#mapping} 
                                    |
Use function {!List.map} to map the | Use function [!List.map] to map the
elements of a list. Other           | elements of a list. Other 
{{!List.iterators}iterators} are    | [iterators][!List.iterators] are 
available.                          | available.
```

And this body of `ocamlmark` warns on the reference definition that it
is illegal, because its label starts with a `!`.

```
See my wonderful [website][!website]

[!website]: https://example.org
```

[CommonMark]: https://spec.commonmark.org/current
[ocamldoc]: https://ocaml.org/manual/ocamldoc.html#ss:ocamldoc-syntax
[ocamldoc-ast]: https://github.com/ocaml-doc/odoc-parser/blob/main/src/ast.ml
[ocamldoc-@tag]: https://ocaml.org/manual/ocamldoc.html#ss:ocamldoc-tags
[pandoc-heading-ids]: https://pandoc.org/MANUAL.html#heading-identifiers

## Translation 

The `ocamldoc` language is defined [here][ocamldoc]
([AST][ocamldoc-ast]).  The `CommonMark` language is defined
[here][commonmark].


<small> The ←, ↔ and → arrows indicate translation directions. </small>
````
ocamldoc                   ocamlmark
--------------------------------------------------------
{[0-5] text}           ↔   #… text
{[0-5]:label text}     ↔   #… text {#label} 
{b text}               ↔   **text**
{i text}               ↔   *text*
{e text}               ↔   *text*
{C text}               ↔   text
{L text}               ↔   text
{R text}               ↔   text
{ol {- }…              ↔   1. …
{ul {- }…              ↔   * … 
{{:url} text}          ↔   [text](url)
{{!ref} text}          ↔   [text][!ref]
[string]               ↔   `string`  (string not starting with `)
[`string]              ↔   `` `string ``
{!ref}                 ↔   [!ref]
{!modules m…}          ↔   [!modules m…] (in its own paragraph)
{!indexlist}           ↔   [!indexlist]  (in its own paragraph)
{^ text}               →   <sup>text</sup>
{%html: …%}            ←   <sup>text</sup>
{_ text}               →   <sub>text</sub>
{%html: …%}            ←   <sub>text</sub>
{[ string ]}           ↔   ```
                           string
                           ```
                           or indented code block
{@info[ string ]}      ↔   ```info 
                           string
                           ```
{v string v}           ↔   ```verb
                           string
                           ```
{%latex: string %}     ↔   ```=latex
{% string %}               string
                           ```
{%html: string %}      ←   HTML block or raw HTML or
                           ```=html
                           string
                           ```
{%texi: string %}      ←   ```=texi
                           string
                           ```
{%man: string %}       ←   ```=man
                           string
                           ```
<img src="url"         ←   ![text](url)
     alt="text">

{%html: <hr/> %}       ←   *** (thematic break) 
???                    ←   > … (block quotes) 

@authors string        ↔   @authors string
@deprecated text       ↔   @deprected text 
@param id text         ↔   @param id text
@raise Exc text        ↔   @raise string text
@return text           ↔   @return text
@see url text          ↔   @see url string
@since string          ↔   @since string
@before string         ↔   @before string
@version string        ↔   @version string 
````

### Documentation comment structure

CommonMark is sensitive to leading spaces on lines and `ocamldoc`
comments are usually indented. 

`ocamlmark` defines a start column to define the first relevant
character of lines in documentation comments. The start column is set
to the first non-space character after the `**` marker (this means right
after it if a newline immediately follows). In this example, the first
character of the line is the `c` character:

    (** c
            This is an indented code block.
           And this is not *)
       
On every line of the comment, leading spaces are trimmed up to the
start column. If there are less spaces than that, the line starts on the 
first non space character as if it was indented to the start column.

These three comments are equivalent: 

```
    (** This is multi-line
        text *)

    (**This is multi-line 
       text *)

    (**   This is multi-line
  text *)  
```

## Notes and unresolved questions

Here are a few notes and questions on the above translation and the
[current implementation] in `odoc-parser` that translates CommonMark to
the `ocamldoc` abstract syntax tree.

 0. Comment structure. We should likely refine the strategy for fenced code 
    blocks, so that we can have them flush left to benefit from 80 columns. 
    For now if you do that you lose indentation up to the start column.

 1. Tags are a litte bit unclear. `odoc` changed their semantics: they
    are no longer allowed inline. We could simply define them as
    paragraphs and the first inline defines the tag.  Personally I
    would rather be in favor of deprecating most them in `ocamlmark`
    and let them be OCaml attributes for the ones that are useful. This
    somehow already happened for `@deprecated` which became an
    alert. `@version`, `@since` and `@before` would also be useful for
    enriching the compiler alerting mecanisms and `@raise` would provide
    information for static analysers. However there are new `odoc`
    specific hint tags that are needed (e.g. `@canonical`, `@open`, 
    `@inline`) and don't make sense as attributes.

 2. The image link reference translation is not really
    statisfactory. After all these years it's time for `odoc` to
    really solve the [page assets problem][page-assets].

 3. The above table does not include the latest `ocamldoc` additions
    (L<sup>A</sup>T<sub>E</sub>X maths) and upcoming additions (tables). 
   `cmarkit` supports both though. 

 4. We need a way to handle the heading 0 level trick of `.mld`
    files. It's not natural to use, for toplevel sections, level 2 in
    `.mld` files and level 1 in modules. The simplest would be for
    `.mld` files to take a first level 1 heading as defining the
    heading 0. But for that we need to know we are parsing a `.mld`
    files, the current `odoc-parser` API does not tell us that.

 5. At the moment `odoc` does not support `ocamldoc`'s [raw HTML
    tags][ocamldoc-html], [no decision][odoc-html] has been taken yet
    but it wouldn't be hard to translate it in either direction. 
   
 6. CommonMark is less restricted than `ocamldoc`. It can have headings
    in list items. When this occurs we warn and drop the construct.

 7. What should we do with thematic breaks ? For now we translated 
    them to raw HTML.
  
 8. Should we treat the concatenation of comments as a document ?
    This only influences link reference definition. Maybe we could
    just accumulate link references as we go. This makes it slightly
    at odds with CommonMark semantics were link definitions are
    allowed to occur anywhere.
   
    But it would be nice to be able to reuse link reference definitions
    from one comment to the other, especially given disadvantage 4 (see
    below). Doing it in reverse (from bottom to top) and accumulating
    refs would allow to have long URLs as link reference definitions at
    the end of the doc.

 9. For the `ocamldoc` to `ocamlmark` translation we don't have a
    syntax for `{%backend:` *when these are used inlines*, except for
    raw HTML if the raw HTML satisfies the complicated contraints of
    CommonMark raw HTML. Could be a problem for that translation
    direction if people used that.

11. Ordered list can specify their starting point in `CommonMark` 
    but can't in `ocamldoc`, we issue a warning.

12. There's no notion of hard breaks in `ocamldoc`, we issue a warning
    to indicate that.

13. For now the super and sub scripts will be translated by virtue of
    raw HTML but this will only show up in the HTML backend.  We could
    try to be smarter on raw HTML and try to parse `<sub>`/`<sup>`
    markup in inlines.
    
14. For simplicity when we get a `[!modules: …]` directive in the
    middle of a paragraph, we drop it. `ocamldoc` is smarter, it warns
    and splits the paragraph. Could be done at the cost of less clear
    control flow, not sure it's a big deal the way it works now, you
    have been warned as they say.

15. `{!indexlist}` is still [unsupported][indexlist] in `odoc` and likely
    forever. For now we recognize it and warn about it.

16. There's no notion of link title in `ocamldoc`, we issue a warning 
    and drop them.

[ocamldoc-html]: https://ocaml.org/manual/ocamldoc.html#sss:ocamldoc-html-tags
[page-assets]: https://github.com/ocaml/odoc/issues/59
[indexlist]: https://github.com/ocaml/odoc/issues/577
[odoc-html]: https://github.com/ocaml/odoc/issues/576
[current implementation]: https://github.com/dbuenzli/odoc-parser/tree/ocamlmark

## Advantages

1. CommonMark is found in a lot of other systems including
   Git{Hub,Lab}, Discourse, etc. It is also increasingly being used
   in API documentation tools, `rustdoc`, `Documenter.jl`, etc.

2. Since it's everywhere, that's one less thing to learn for newcomers and 
   one less thing to have in the head for old-timers. But *only if* we 
   eventually get rid of the `ocamldoc` syntax. 

3. `.mld` files become (slightly special) `.md` files which means they
   get a bit of rendering in various systems like Git{Lab,Hub} etc. As
   per CommonMark specification, `ocamldoc` references will become
   plain text in these renders, so we get "graceful degradation". We
   can also lean on the CommonMark support of editors to edit them.

4. One of the stated philosophy of CommonMark is that its formatting
   should be publishable as-is without looking like it's been marked
   up. This looks like a good fit for documentation in programming
   language comments.


## Disadvantages

1. CommonMark never errors. If you get your markup wrong, you just get
   text. You may not realize your outputs are garbled if you don't
   check the renderings.

2. If the `ocamldoc` language is kept. You now have two syntaxes for
   writing documentation. Who can possibly think that syntax choice
   is good ?

3. The syntax for inline code clashes with the backtick of polymorphic 
   variants. If you want to inline a polymorphic variant case you need
   to write, ``` `` `Myvariant `` ``` which is quite annoying to 
   remember, read and write.

4. CommonMark has no syntax to break long URLs. Neither has `ocamldoc` 
   but I [hope] it can be added.
   
   [hope]: https://github.com/ocaml/odoc/issues/865

5. CommonMark is a rather complicated beast to parse full of weird
   corner and pathological cases. This could make some tooling more
   complicated than it needs to be (e.g. `ocp-indent` or `merlin` which
   currenly completes in comments ? `odoc-parser` could mediate that 
   though).

6. OCaml's doc comments becomes constrained by the CommonMark
   specification and tied to what CommonMark can be. If `ocamlmark`
   deviates too much, benefits are lost.


## Personal opinions

1. Disadvantage 2. should really be taken seriously, it basically thwarts
   advantage 2.  If both syntax are offered and officially supported then
   as a user of the eco-system I have to a) choose one b) still learn the
   other, since others will have made a different choice.

   If `ocamlmark` is to be pursued, it should become the official
   language, `ocamldoc` should be deprecated and we should devise
   source-to-source comment migration tool so code bases can gradually
   move to it effortlessly. Nothing insurmountable.

2. I'm also a bit concerned by disadvantage 1. – personally I mostly read
   the renderings when I devise docs but I suspect many people don't, 
   could be a problem for doc QA.

3. To really assess advantage 4. I'd need more than the toy examples I
   played with for making the POC. Does it really *look* good ? 
   
   One thing I missed is the easily scannable `ocamldoc` headers and
   their labels. But maybe better syntax highlighting in comments
   could fix that.
   
   In general I find the `ocamldoc` code span syntax much more
   readable:
   
   * `` `'a` ``  vs `['a]`
   * `` `'a'` `` vs `['a']`
   * `` `"string"` `` vs `["string"]`
   * ``` `` `A `` ``` vs ``[`A]``
