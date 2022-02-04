Markup

Module `Markup`

Here, we test the rendering of comment markup.

# Sections

Let's get these done first, because sections will be used to break up the rest of this test.

Besides the section heading above, there are also

## Subsection headings

---

and

### Sub-subsection headings

---

but odoc has banned deeper headings. There are also title headings, but they are only allowed in mld files.

### Anchors

---

Sections can have attached [Anchors](#anchors) , and it is possible to [link](#anchors) to them. Links to section headers should not be set in source code style.

#### Paragraph

---

Individual paragraphs can have a heading.

##### Subparagraph

---

Parts of a longer paragraph that can be considered alone can also have headings.

# Styling

This paragraph has some styled elements: **bold** and _italic_ , **_bold italic_** , _emphasis_ , __emphasis_ within emphasis_ , **_bold italic_** , super <sup>script</sup> , sub <sub>script</sub> . The line spacing should be enough for superscripts and subscripts not to look odd.

Note: _In italics _emphasis_ is rendered as normal text while _emphasis _in_ emphasis_ is rendered in italics._ _It also work the same in [links in italics with _emphasis _in_ emphasis_ .](#)_

`code` is a different kind of markup that doesn't allow nested markup.

It's possible for two markup elements to appear **next** _to_ each other and have a space, and appear **next** _to_ each other with no space. It doesn't matter **how** _much_ space it was in the source: in this sentence, it was two space characters. And in this one, there is **a** _newline_ .

This is also true between _non-_ `code` markup _and_ `code` .

Code can appear **inside `other` markup** . Its display shouldn't be affected.

# Links and references

This is a [link](#) . It sends you to the top of this page. Links can have markup inside them: [**bold**](#) , [_italics_](#) , [_emphasis_](#) , [super <sup>script</sup>](#) , [sub <sub>script</sub>](#) , and [`code`](#) . Links can also be nested _[inside](#)_ markup. Links cannot be nested inside each other. This link has no replacement text: [#](#) . The text is filled in by odoc. This is a shorthand link: [#](#) . The text is also filled in by odoc in this case.

This is a reference to [`foo`](#val-foo) . References can have replacement text: [the value foo](#val-foo) . Except for the special lookup support, references are pretty much just like links. The replacement text can have nested styles: [**bold**](#val-foo) , [_italic_](#val-foo) , [_emphasis_](#val-foo) , [super <sup>script</sup>](#val-foo) , [sub <sub>script</sub>](#val-foo) , and [`code`](#val-foo) . It's also possible to surround a reference in a style: **[`foo`](#val-foo)** . References can't be nested inside references, and links and references can't be nested inside each other.

# Preformatted text

This is a code block:

```
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

# Lists

- This is a

- shorthand bulleted list,

- and the paragraphs in each list item support _styling_ .

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
  
  - [`foo`](#val-foo)

# Unicode

The parser supports any ASCII-compatible encoding, in particuλar UTF-8.

# Raw HTML

Raw HTML can be <input type="text" placeholder="inserted"> as inline elements into sentences.


    <blockquote>
      If the raw HTML is the only thing in a paragraph, it is treated as a block
      element, and won't be wrapped in paragraph tags by the HTML generator.
    </blockquote>
    

# Modules

@[`X`](Markup.X.md):

@[`X`](Markup.X.md):

@[`Y`](Markup.Y.md):

# Tags

Each comment can end with zero or more tags. Here are some examples:

@author: antron

@deprecated:

@parameter foo:

@raises Failure:

@returns:

@see [#](#):

@see `foo.ml`:

@see Foo:

@since: 0

@before 1.0:

@version: -1

<a id="val-foo"></a>

###### val foo :

> unit


Comments in structure items **support** _markup_ , t <sup>o</sup> <sub>o</sub> .

Some modules to support references.

<a id="module-X"></a>

###### module [X](Markup.X.md)

<a id="module-Y"></a>

###### module [Y](Markup.Y.md)