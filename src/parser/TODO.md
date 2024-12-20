- Leading white-space on tags
  - Tests expect tag bodies (for i.e. "@version" or "@canonical", Lexer line 610) to have no 
    leading white-space. Fiddling with the lexer's rules to fix this seems to 
    break its ability to match on these rules correctly.
  - Worst case we trim the start of the tag body
- Locations
  - Not sure what the problem is here, not specifically at least
  - The lexer has a, maybe unnecessarily, complex location handling system 
    with lots of curried functions, which was written with the intention of 
    wrapping tokens in `Loc.with_location`         
  - Do we rewrite it from scratch? (~week of work)
  - Or fix it as it is now
- Code blocks 
  - Code blocks do not work now. The lexer relies on cooperation from the parser
    which is not possible with Menhir. 
  - Our only option is to break the `Code_block` token up in multiple tokens,
    the question is how:                                                 
      - Do we split it into `Code_block` and `Code_block_w_output`? 
      - Or into its delimiters, i.e. `RIGHT_CODE_BLOCK`, `RIGHT_BRACKET`, 
        `CODE_BLOCK_META` etc?
      - Seeing as the AST expects a string for its content I can see a benefit
        in parsing said content within the lexer
