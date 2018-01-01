module Identifier = Model.Paths.Identifier
module Comment = Model.Comment

type 'a with_location = 'a Model.Location_.with_location



type block_element = [
  | Comment.nestable_block_element
  | `Heading of int * Identifier.label option * Comment.link_content
  | `Tag of Comment.tag
]

type docs = (block_element with_location) list



type sections_allowed = [ `All | `No_titles | `None ]
