----------- Names ---------------------------------------------

Without --name argument, all **OCaml** code blocks are extracted

  $ odoc extract-code main.mld
  
  (** By default, an odoc code block is assumed to contain OCaml code *)
  let () = ()
  let x = 5
     let () = print_int x
    
     let y = x + 6. (* This is a typing error *)
     

With --name argument, language does not matter

  $ odoc extract-code --name c-quine main.mld
  
  #include <stdio.h>
  int main(){
  char*a="#include <stdio.h>%cint main(){char*a=%c%s%c;printf(a,10,34,a,34);}";
  printf(a,10,34,a,34);
  }

Multiple name can be given

  $ odoc extract-code --line-directives --name error.ml --name printing main.mld
  #18 "main.mld"
                                       let x = 5
  #20 "main.mld"
                            
     let () = print_int x
    
  #25 "main.mld"
                            
     let y = x + 6. (* This is a typing error *)
     

------- Line directives ---------------------------------------

We can add (OCaml) line directives

  $ odoc extract-code --line-directives main.mld
  #5 "main.mld"
     
  (** By default, an odoc code block is assumed to contain OCaml code *)
  let () = ()
  
  #18 "main.mld"
                                       let x = 5
  #20 "main.mld"
                            
     let () = print_int x
    
  #25 "main.mld"
                            
     let y = x + 6. (* This is a typing error *)
     

Let's restrict to a named code blocks

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
     

Let's check line directive work ("-color always" to make sure the error message
is the same in all context):

  $ ocaml -color always error.ml
  File "main.mld", line 26, characters 15-17:
  Error: This expression has type float but an expression was expected of type
           int
  [2]

Here is the line 26, and the characters 15-17:

  $ sed -n '26p' main.mld
     let y = x + 6. (* This is a typing error *)
  $ sed -n '26p' main.mld | cut -c15-17
   6.
