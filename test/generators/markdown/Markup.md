
# Module `Markup`

Here, we test the rendering of comment markup.


## Sections

Let's get these done first, because sections will be used to break up the rest of this test.

Besides the section heading above, there are also


### Subsection headings

and


#### Sub-subsection headings

but odoc has banned deeper headings. There are also title headings, but they are only allowed in mld files.


#### Anchors

Sections can have attached [Anchors](./#anchors), and it is possible to [link](./#anchors) to them. Links to section headers should not be set in source code style.


##### Paragraph

Individual paragraphs can have a heading.


###### Subparagraph

Parts of a longer paragraph that can be considered alone can also have headings.


## Styling

This paragraph has some styled elements: **bold** and *italic*, ***bold italic***, *emphasis*, **emphasis* within emphasis*, ***bold italic***, superscript, subscript. The line spacing should be enough for superscripts and subscripts not to look odd.

Note: *In italics *emphasis* is rendered as normal text while *emphasis *in* emphasis* is rendered in italics.* *It also work the same in [links in italics with *emphasis *in* emphasis*.](#)*

`code` is a different kind of markup that doesn't allow nested markup.

It's possible for two markup elements to appear **next** *to* each other and have a space, and appear **next***to* each other with no space. It doesn't matter **how** *much* space it was in the source: in this sentence, it was two space characters. And in this one, there is **a** *newline*.

This is also true between *non-*`code` markup *and* `code`.

Code can appear **inside `other` markup**. Its display shouldn't be affected.

There is no differences between `a b` and `a b`.

Consecutive whitespaces not after a newline are conserved as they are: `a   b`.


## Links and references

This is a [link](#). It sends you to the top of this page. Links can have markup inside them: [**bold**](#), [*italics*](#), [*emphasis*](#), [superscript](#), [subscript](#), and [`code`](#). Links can also be nested *[inside](#)* markup. Links cannot be nested inside each other. This link has no replacement text: [\#](#). The text is filled in by odoc. This is a shorthand link: [\#](#). The text is also filled in by odoc in this case.

This is a reference to [`foo`](./#val-foo). References can have replacement text: [the value foo](./#val-foo). Except for the special lookup support, references are pretty much just like links. The replacement text can have nested styles: [**bold**](./#val-foo), [*italic*](./#val-foo), [*emphasis*](./#val-foo), [superscript](./#val-foo), [subscript](./#val-foo), and [`code`](./#val-foo). It's also possible to surround a reference in a style: **[`foo`](./#val-foo)**. References can't be nested inside references, and links and references can't be nested inside each other.


## Preformatted text

This is a code block:

```ocaml
    let foo = ()
    (** There are some nested comments in here, but an unpaired comment
        terminator would terminate the whole doc surrounding comment. It's
        best to keep code blocks no wider than 72 characters. *)

    let bar =
      ignore foo
```
There are also verbatim blocks:

```
The main difference is these don't get syntax highlighting.
```

## Lists

- This is a
- shorthand bulleted list,
- and the paragraphs in each list item support *styling*.
1. This is a
2. shorthand numbered list.
- Shorthand list items can span multiple lines, however trying to put two paragraphs into a shorthand list item using a double line break
just creates a paragraph outside the list.

- Similarly, inserting a blank line between two list items
- creates two separate lists.
- To get around this limitation, one
  
  can use explicitly-delimited lists.
  
- This one is bulleted,
1. but there is also the numbered variant.
- - lists
  - can be nested
  - and can include references
  - [`foo`](./#val-foo)

## Unicode

The parser supports any ASCII-compatible encoding.

In particuλar UTF-8.


## Raw HTML

Raw HTML can be <input type="text" placeholder="inserted"> as inline elements into sentences.


    <blockquote>
      If the raw HTML is the only thing in a paragraph, it is treated as a block
      element, and won't be wrapped in paragraph tags by the HTML generator.
    </blockquote>
    

## Math

Math elements can be inline: `\int_{-\infty}^\infty`, or blocks:

```
    % \f is defined as #1f(#2) using the macro
    \newcommand{\f}[2]{#1f(#2)}
    \f\relax{x} = \int_{-\infty}^\infty
    \f\hat\xi\,e^{2 \pi i \xi x}
    \,d\xi
    
```

## Modules

[`X`](./Markup-X.md) 
[`X`](./Markup-X.md) 
[`Y`](./Markup-Y.md) 

## Tables

| Left | Center | Right | Default |
| :-- | :-: | --: | --- |
| A | B | C | D |
| Left | Center | Right | Default |
| :-- | :-: | --: | --- |
| A | B | C | D |
| A much longer paragraph which will need to be wrapped and more content and more content and some different content and we will see what is does if we can see it | B much longer paragraph which will need to be wrapped and more content and more content and some different content and we will see what is does if we can see it | C much longer paragraph which will need to be wrapped and more content and more content and some different content and we will see what is does if we can see it | D much longer paragraph which will need to be wrapped and more content and more content and some different content and we will see what is does if we can see it |
| --- | --- |
| No | Header |
| A | B |
| Header 1 | Header 2 |
| --- | --- |
| Data 1 | Data 2 |
| Header 1 | Data 1 |
| Header 2 | Data 2 |

## Tags

Each comment can end with zero or more tags. Here are some examples:

author antron
deprecated a long time ago
parameter foo unused
raises `Failure` always
returns never
see [\#](#) this url
see `foo.ml` this file
see Foo this document
since 0
before 1\.0 it was in beta
version \-1
```
val foo : unit
```
Comments in structure items **support** *markup*, too.

Some modules to support references.

```
module X : sig ... end
```
```
module Y : sig ... end
```