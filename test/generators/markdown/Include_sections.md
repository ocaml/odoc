Include_sections

Module `Include_sections`

<a id="module-type-Something"></a>

###### module type [Something](Include_sections.module-type-Something.md)

A module type.

Let's include [`Something`](Include_sections.module-type-Something.md) once

# Something 1

foo

## Something 2

# Something 1-bis

Some text.

# Second include

Let's include [`Something`](Include_sections.module-type-Something.md) a
second time: the heading level should be shift here.

# Something 1

foo

## Something 2

# Something 1-bis

Some text.

## Third include

Shifted some more.

# Something 1

foo

## Something 2

# Something 1-bis

Some text.

And let's include it again, but without inlining it this time: the ToC
shouldn't grow.

<a id="val-something"></a>

###### val something :

> unit

# Something 1

foo

<a id="val-foo"></a>

###### val foo :

> unit

## Something 2

<a id="val-bar"></a>

###### val bar :

> unit

foo bar

# Something 1-bis

Some text.
