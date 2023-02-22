We'll now try constructing a more complicated tree.

We'll have two top pages this time:

  $ cat top1.mld
  {0 Top1}
  This is the top1 page.
  $ cat top2.mld
  {0 Top2}
  This is the top2 page

And some sub-pages, and we'll make sub3 a sub-page of sub1.
  $ cat sub1.mld
  {0 Sub1}
  This is sub1

  $ cat sub2.mld
  {0 Sub2}
  This is sub2

  $ cat sub3.mld
  {0 Sub3}
  This is Sub3

And let's have a few modules too - these will be parented under sub1,2 and 3 respectively.

  $ cat m1.mli
  type m1t = int
  
  $ cat m2.mli
  type m2t = float
  
  $ cat m3.mli
  type m3t = string
  

Compile the modules:
  $ ocamlc -c m1.mli -bin-annot
  $ ocamlc -c m2.mli -bin-annot
  $ ocamlc -c m3.mli -bin-annot

Now compile the pages:
  $ odoc compile top1.mld --child page-sub1
  $ odoc compile top2.mld --child page-sub2
  $ odoc compile sub1.mld -I . --parent top1 --child page-sub3 --child M1
  $ odoc compile sub2.mld -I . --parent top2 --child M2
  $ odoc compile sub3.mld -I . --parent sub1 --child M3
  $ odoc compile m1.cmti -I . --parent sub1
  $ odoc compile m2.cmti -I . --parent sub2
  $ odoc compile m3.cmti -I . --parent sub3

Now link everything:
  $ for i in *.odoc; do odoc link -I . $i; done

And output the HTML:
  $ for i in *.odocl; do odoc html-generate -o html $i; done

Let's see what we've got!
  $ find html -type f | sort
  html/top1/index.html
  html/top1/sub1/M1/index.html
  html/top1/sub1/index.html
  html/top1/sub1/sub3/M3/index.html
  html/top1/sub1/sub3/index.html
  html/top2/index.html
  html/top2/sub2/M2/index.html
  html/top2/sub2/index.html
