- How to solve combinatorial explosion in elements like lists and tables?
  - Ex: in a "heavy" list, an illegal token can exist in every nesting level
    - {ul Something_illegal {li foo} {li bar} }
    - {ul {li foo} Something_illegal {li bar} }
    - {ul {li foo} {li bar Something_illegal} }
  - Matching on each possible combination not only requires a great deal of
    effort and repeated code, it weakens Menhir's ability to reduce the correct
    rule
  - My thought is that we do something like this: 
    ```ocaml
      let list_error_cases := 
        | List; items = list_items+; loc = located(error);
          { Warning.unexpected_at loc.location }
        | List; loc = located(error);
          { (* Same as above *)}
    ```
      - With this strategy, we lose some information, but if we modify our
        warning type (currently a function from filename to warning) to be a
        function which takes the input text, we can at least show the user the specific
        span that is illegal by mapping over our warnings and evaluating them.

      - So this warning looks something like 
        ```ocaml
          fun ~filename input ->
            String.split_on_char '\n' input 
            |> get_error_span location
            |> Warning.unexpected_at location
             
        ```
