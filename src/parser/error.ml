type parser_error = 
  | Unclosed of 
    { opening : string 
    ; items : string 
    ; closing : string 
    }
  | Expecting of string

exception Parser_error of parser_error Loc.with_location 
