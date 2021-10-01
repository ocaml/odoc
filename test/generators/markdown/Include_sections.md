Include_sections

Module  `` Include_sections `` 

###### module type Something = sig

######     val something : unit


## Something 1
---

foo
######     val foo : unit


### Something 2
---

######     val bar : unit

foo bar

## Something 1-bis
---

Some text.

###### end

A module type.
Let's include  `` Something ``  once

# Something 1

foo

## Something 2
---


# Something 1-bis

Some text.

# Second include

Let's include  `` Something ``  a second time: the heading level should be shift here.

## Something 1
---

foo

### Something 2
---


## Something 1-bis
---

Some text.

## Third include
---

Shifted some more.

### Something 1
---

foo

#### Something 2
---


### Something 1-bis
---

Some text.
And let's include it again, but without inlining it this time: the ToC shouldn't grow.
###### val something : unit


### Something 1
---

foo
###### val foo : unit


#### Something 2
---

###### val bar : unit

foo bar

### Something 1-bis
---

Some text.
