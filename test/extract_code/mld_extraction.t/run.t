----------- Names ---------------------------------------------

Without --name argument, all **OCaml** code blocks are extracted

  $ odoc extract-code main.mld
  
  (** By default, an odoc code block is assumed to contain OCaml code *)
  let () = ()
  let five = 5
     let () = print_int five
    
     let y = five +. five (* This is a typing error *)
     

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
                                      let five = 5
  #20 "main.mld"
                           
     let () = print_int five
    
  #25 "main.mld"
                           
     let y = five +. five (* This is a typing error *)
     

------- Line directives ---------------------------------------

We can add (OCaml) line directives

  $ odoc extract-code --line-directives main.mld
  #5 "main.mld"
    
  (** By default, an odoc code block is assumed to contain OCaml code *)
  let () = ()
  
  #18 "main.mld"
                                      let five = 5
  #20 "main.mld"
                           
     let () = print_int five
    
  #25 "main.mld"
                           
     let y = five +. five (* This is a typing error *)
     

Let's restrict to a named code blocks

  $ odoc extract-code --line-directives --name error.ml main.mld
  #18 "main.mld"
                                      let five = 5
  #25 "main.mld"
                           
     let y = five +. five (* This is a typing error *)
     

We can output to a file

  $ odoc extract-code --line-directives --name error.ml -o error.ml main.mld
  $ cat error.ml
  #18 "main.mld"
                                      let five = 5
  #25 "main.mld"
                           
     let y = five +. five (* This is a typing error *)
     

Let's check line directive work (we only look at the location to avoid ocaml
version-dependent output):

  $ ocaml error.ml 2> error.txt
  [2]
  $ grep File error.txt
  File "main.mld", line 26, characters 11-15:

Here is the line 26, and the characters 11-15:

  $ sed -n '26p' main.mld
     let y = five +. five (* This is a typing error *)
  $ sed -n '26p' main.mld | cut -c11-15
   five

"five" has type int and should have type float.
