Nested

Module  `` Nested `` 

This comment needs to be here before #235 is fixed.

# Module

###### module X : sig


# Type

######     type t

Some type.

# Values

######     val x : t

The value of x.

###### end

This is module X.

# Module type

###### module type Y = sig


### Type
---

######     type t

Some type.

### Values
---

######     val y : t

The value of y.

###### end

This is module type Y.

# Functor

###### module F : sig


# Type


# Parameters

######     module Arg1 : sig


# Type

######         type t

Some type.

# Values

######         val y : t

The value of y.

######     end

######     module Arg2 : sig


# Type

######         type t

Some type.

######     end


# Signature

######     type t = Arg1.t * Arg2.t

Some type.

###### end

This is a functor F.

# Class

###### class virtual  z : object

######     val y : int

Some value.
######     val mutable virtual y' : int


# Methods

######     method z : int

Some method.
######     method private virtual z' : int


###### end

This is class z.
###### class virtual  inherits : object

######     inherit z


###### end

