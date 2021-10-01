(** {1 Section 1}

    {2 Example}
 
    The first definition of the label 'example'.
 
    {1 Section 2}
 
    {2 Example}
 
    The second definition of 'example', this label can't be referenced and a
    warning will be generated.
    Internally, a unique name will be used to make the TOC work.
 
    Nesting has no effect, this will reference 'example' inside 'section 1':
    {{!example} Should resolve to the first label}

    {2 Example}

    {2 Example_3}

    This one will collide with the internal name used for the TOC.

    It is not possible to use the internal name in references:
    {{!example_2} Shouldn't resolve} *)

let x = 1
  
