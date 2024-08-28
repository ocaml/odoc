type media = Reference of string | Link of string 
type media_target = Audio | Video | Image


type tag =  
    Author of string
  | Deprecated
  | Param of string 
  | Raise of string 
  | Return
  | See of [ `Url | `File | `Document ] * string
  | Since of string 
  | Before of string 
  | Version of string 
  | Canonical of string 
  | Inline 
  | Open 
  | Closed 
  | Hidden 
