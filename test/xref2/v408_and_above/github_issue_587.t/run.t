A quick test to repro the issue found in #587

  $ ./build.sh
  File "odoc_bug__a_intf.cmt":
  Failed to compile expansion for module type expression identifier((root Odoc_bug__a_intf).S, false) Unresolved module type path identifier((root Odoc_bug__a_intf).S, false) (Lookup failure (module type): (root Odoc_bug__a_intf).S)
  File "odoc_bug__a_intf.cmt":
  Failed to compile expansion for module type expression identifier((root Odoc_bug__a_intf).S, false) Unresolved module type path identifier((root Odoc_bug__a_intf).S, false) (Lookup failure (module type): (root Odoc_bug__a_intf).S)
  File "odoc_bug__a_intf.cmt":
  Failed to compile expansion for module type expression identifier((root Odoc_bug__a_intf).S, false) Unresolved module type path identifier((root Odoc_bug__a_intf).S, false) (Lookup failure (module type): (root Odoc_bug__a_intf).S)
  File "odoc_bug__a_intf.cmt":
  Failed to compile expansion for module type expression identifier((root Odoc_bug__a_intf).S, false) Unresolved module type path identifier((root Odoc_bug__a_intf).S, false) (Lookup failure (module type): (root Odoc_bug__a_intf).S)
  File "odoc_bug__b_intf.cmt":
  Failed to compile expansion for module type expression identifier((root Odoc_bug__b_intf).S, false) Unresolved module type path identifier((root Odoc_bug__b_intf).S, false) (Lookup failure (module type): (root Odoc_bug__b_intf).S)
  File "odoc_bug__b_intf.cmt":
  Failed to compile expansion for module type expression identifier((root Odoc_bug__b_intf).S, false) Unresolved module type path identifier((root Odoc_bug__b_intf).S, false) (Lookup failure (module type): (root Odoc_bug__b_intf).S)
  File "odoc_bug__b_intf.cmt":
  Failed to compile expansion for include : identifier((root Odoc_bug__b_intf).B.S, false) Unresolved module type path identifier((root Odoc_bug__b_intf).S, false) (Lookup failure (module type): (root Odoc_bug__b_intf).S)
  File "odoc_bug__b_intf.cmt":
  Failed to compile expansion for module type expression identifier((root Odoc_bug__b_intf).S, false) Unresolved module type path identifier((root Odoc_bug__b_intf).S, false) (Lookup failure (module type): (root Odoc_bug__b_intf).S)
  File "odoc_bug__b_intf.cmt":
  Failed to compile expansion for module type expression identifier((root Odoc_bug__b_intf).S, false) Unresolved module type path identifier((root Odoc_bug__b_intf).S, false) (Lookup failure (module type): (root Odoc_bug__b_intf).S)
  File "odoc_bug__b_intf.cmt":
  Failed to compile expansion for include : identifier((root Odoc_bug__b_intf).B.S, false) Unresolved module type path identifier((root Odoc_bug__b_intf).S, false) (Lookup failure (module type): (root Odoc_bug__b_intf).S)


