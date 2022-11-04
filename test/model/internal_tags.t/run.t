Test handling of internal tags.
We expect no warning for "good.mli". The code already ensures that either tags
are handled of a warning is emitted.

  $ compile good.mli

We expect warnings to be emitted for each bad tags:

  $ compile bad.mli
  File "bad.mli", line 3, characters 4-11:
  Warning: Unexpected tag '@inline' at this location.
  File "bad.mli", line 7, characters 4-19:
  Warning: Unexpected tag '@canonical' at this location.
  File "bad.mli", line 12, characters 4-19:
  Warning: Unexpected tag '@canonical' at this location.
  File "bad.mli", line 17, characters 4-19:
  Warning: Unexpected tag '@canonical' at this location.
  File "bad.mli", line 21, characters 4-11:
  Warning: Unexpected tag '@inline' at this location.
