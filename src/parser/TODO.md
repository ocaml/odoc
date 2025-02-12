- Some locations are still not accurate. This seems to be acting up in comments that span 
  many lines. There is potentially an off-by-one error or similar in 
  `Lexer.update_content_newlines` which is (supposed) to increment the lexbuf's line 
  position for every newline encountered in some content (i.e. inside of a code or math block)

- Top-level errors like two nestable block elements or headings on the same line
  need to be handled. Currently, they parse correctly but do not emit a warning. 

- Repetition in `tag_with_content` parse rule(parser.mly:207). Two productions are identical 
  save for a newline. This is because an optional newline causes a reduce conflict due to 
  `nestable_block_element`'s handling of whitespace.

- Improve error handling inside light table cells. Currently, we cannot do much besides use 
  Menhir's `error` token, which erases all information about the error which happened and we 
  have to use a string of the offending token to display what went wrong to users, which 
  doesn't necessarily communicate a lot

- Tests. There are a few tests, like the ones which test the positions in the lexing buffer,
  which don't apply to the new parser. Others expect error messages which cannot be produced
  by the relevant parser rule

- Likely some error cases which have not been handled. These should be trivial to fix, 
  you should really only need to add a new production to the relevant parser rule which 
  handles the offending token

Notes for anyone working on this
- Due to the nature of Menhir, this parser is difficult to work on. 
  - Changes will have unexpected non-local consequences due to more or less tokens being consumed by 
    some neighboring (in the parse tree) rule. 
  - You need to familiarize yourself with the branch of the parse tree that you're working on 
    (i.e. toplevel->nestable_block_element->paragraph) before you start making non-trivial changes.
  - Type errors will point towards unrelated sections of the parser or give you incorrect information 
    about what has gone wrong. 

- If you need to emulate some sort of context like "paragraphs can't accept '|' tokens if they're inside 
  tables", then you need to parameterize that rule by some other rule which dictates what it can accept. 
  For example, toplevel block elements match `paragraph(any_symbol)` and tables match 
  `paragraph(symbols_except_bar)`

- Be as specific as possible. Avoid optional tokens when possible. Prefer the non-empty
  list rules (`sequence_nonempty`, `sequence_separated_nonempty`) over the alternatives. 
  Ambiguity will produce a compile-time reduce/reduce rule if you're lucky, unexpected 
  behavior at runtime if you're not.

- Contact me on the company slack or at faycarsons23@gmail.com if you're confused about 
  anything!
