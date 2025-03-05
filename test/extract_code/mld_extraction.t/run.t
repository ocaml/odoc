Without --name argument, all OCaml code blocks are extracted

  $ odoc extract-code main.mld
  let x = 5
     let () = print_int x
    
     let y = x + 6. (* This is a typing error *)
     

We can add (OCaml) line directives

  $ odoc extract-code --line-directives main.mld
  #18 "main.mld"
                                       let x = 5
  #20 "main.mld"
                            
     let () = print_int x
    
  #25 "main.mld"
                            
     let y = x + 6. (* This is a typing error *)
     

We can restrict to a named code blocks

  $ odoc extract-code --line-directives --name error.ml main.mld
  #18 "main.mld"
                                       let x = 5
  #25 "main.mld"
                            
     let y = x + 6. (* This is a typing error *)
     

We can output to a file

  $ odoc extract-code --line-directives --name error.ml -o error.ml main.mld
  $ cat error.ml
  #18 "main.mld"
                                       let x = 5
  #25 "main.mld"
                            
     let y = x + 6. (* This is a typing error *)
     

Let's check line directive work:

  $ ocaml error.ml
  File "main.mld", line 26, characters 15-17:
  Error: This expression has type "float" but an expression was expected of type
           "int"
  [2]

Here is the line 26, and the characters 15-17:

  $ sed -n '26p' main.mld
     let y = x + 6. (* This is a typing error *)
  $ sed -n '26p' main.mld | cut -c15-17
   6.

We can get content from multiple names

  $ odoc extract-code --line-directives --name error.ml --name printing main.mld
  #18 "main.mld"
                                       let x = 5
  #20 "main.mld"
                            
     let () = print_int x
    
  #25 "main.mld"
                            
     let y = x + 6. (* This is a typing error *)
     
  $ odoc extract-code --line-directives --name error.ml --name printing -o names.ml main.mld
  $ ocaml names.ml
  File "main.mld", line 26, characters 15-17:
  Error: This expression has type "float" but an expression was expected of type
           "int"
  5
  [2]
