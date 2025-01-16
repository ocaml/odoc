- Some locations are still not accurate, all necessary tokens should pass 
  starting location so this should be trivial to fix
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
