The warning only shows up for explicitly defined labels.

  $ compile test.mli
  Duplicate found: (root Test).foo
  File "test.mli", line 5, characters 4-14:
  Label 'foo' is ambiguous. The other occurences are:
    File "test.mli", line 5, character 4
    File "test.mli", line 3, character 4
  File "test.mli", line 3, characters 4-14:
  Label 'foo' is ambiguous. The other occurences are:
    File "test.mli", line 5, character 4
    File "test.mli", line 3, character 4
