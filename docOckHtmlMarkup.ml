open Html5.M

let keyword keyword = span ~a:[ a_class ["keyword"] ] [ pcdata keyword ]

let def_div lst = div ~a:[ a_class ["def" ] ] lst

let def_summary lst = summary [ span ~a:[ a_class ["def"] ] lst ]

let anchor_region_div ~id content =
  div ~a:[ a_class ["region" ] ; a_id id  ] (
    a ~a:[ a_href ("#" ^ id) ; a_class ["anchor"] ] [] ::
    content
  )
