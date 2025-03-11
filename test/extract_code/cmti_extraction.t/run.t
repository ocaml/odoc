  $ ocamlc -bin-annot main.mli
  $ odoc extract-code -o output.ml --line-directives main.cmti

  $ cat output.ml
  #1 "main.mli"
        
        let x = 1
      
  #7 "main.mli"
        
        let () =
          print_int x;
          print_newline ()
      
  #15 "main.mli"
          
          let hello = 2
        
  #22 "main.mli"
              
              let _ = hello +. hello
            

  $ ocaml output.ml 2> error.txt
  1
  [2]
  $ grep File error.txt
  File "main.mli", line 23, characters 20-25:
