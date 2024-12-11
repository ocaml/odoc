This is directory meants for test on a whole switch. We only test that we can
actually build a documentation database in for every package. We do not check
that the results of search are good, because we do not have a definition of that
for any package.

It has two scripts :

- `set_big_switch.sh` installs a lot of compatible packages in the current
  switch.
- `test.sh` generates the search database of every installed package. Its output
  is in the `packages` folder.