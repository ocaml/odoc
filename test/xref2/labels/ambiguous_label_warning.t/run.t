The warning only shows up for explicitly defined labels.

  $ compile test.mli
  File "test.mli", line 11, characters 4-14:
  Warning: Multiple sections named 'heading' found. Please alter one to ensure reference is unambiguous. Locations:
    File "test.mli", line 7, character 4
    File "test.mli", line 9, character 4
  File "test.mli", line 5, characters 4-14:
  Warning: Label 'foo' is ambiguous. The other occurences are:
    File "test.mli", line 3, character 4
  File "test.mli", line 3, characters 4-14:
  Warning: Label 'foo' is ambiguous. The other occurences are:
    File "test.mli", line 5, character 4
