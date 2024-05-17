  $ odoc compile my_page.mld
  $ mkdir -p a/b/c/
  $ mkdir -p z
  $ cp page-my_page.odoc a/b/c/

  $ odoc compile file.mld

  $ odoc link -P pkg:a/ page-file.odoc

Testing the collision detection:

Same directory used twice
  $ odoc link -P pkg:a/ -P pkg2:a/ page-file.odoc 2>&1 >/dev/null | grep Attention
        Failure("Attention: ce n'est pas une antichaine TODO erreur message yo")

# Two directories given relatively
 Right input:
  $ odoc link -P pkg:a/ -P pkg2:z/ page-file.odoc
 Wrong input:
  $ odoc link -P pkg:a/ -P pkg2:a/b page-file.odoc 2>&1 >/dev/null | grep Attention
        Failure("Attention: ce n'est pas une antichaine TODO erreur message yo")

# One directory given relatively, the other absolutely
 Right input
  $ odoc link -P pkg:a/ -P pkg2:$PWD/z/ page-file.odoc
 Wrong input
  $ odoc link -P pkg:a/ -P pkg2:$PWD/a/b page-file.odoc 2>&1 >/dev/null | grep Attention
        Failure("Attention: ce n'est pas une antichaine TODO erreur message yo")

# Two directories given absolutely
 Right input
  $ odoc link -P pkg:$PWD/a/ -P pkg2:$PWD/z/ page-file.odoc
 Wrong input
  $ odoc link -P pkg:$PWD/a/ -P pkg2:$PWD/a/b page-file.odoc 2>&1 >/dev/null | grep Attention
        Failure("Attention: ce n'est pas une antichaine TODO erreur message yo")

# With a bit of relative faff
 Right input:
  $ odoc link -P pkg:a/../a -P pkg2:z/../z page-file.odoc
 Wrong input:
  $ odoc link -P pkg:a/../a -P pkg2:z/../a/b page-file.odoc 2>&1 >/dev/null | grep Attention
        Failure("Attention: ce n'est pas une antichaine TODO erreur message yo")
