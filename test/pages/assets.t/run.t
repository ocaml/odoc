Blablabla

  $ cat index.mld
  {0 Package page}
  
  Some image:
  {%html: <img src="img.jpg" />%}

And we'll have a module that we'll put underneath this package page.

  $ cat test.mli 
  (** Humpf, let's try accessing the asset:
    {%html: <img src="../img.jpg" />%}
    *)
  
  (** Nevermind *)
  type t
  

Compile the module first

  $ ocamlc -c -bin-annot test.mli

Then we need to odoc-compile the package mld file, listing its children

  $ odoc compile index.mld --child module-test --child asset-img.jpg

This will have produced a file called 'page-index.odoc'.
Now we can odoc-compile the module odoc file passing that file as parent.

  $ odoc compile test.cmti -I . --parent index

Link and generate the HTML (forgetting the asset!):

  $ for i in *.odoc; do odoc link -I . $i; done
  $ for i in *.odocl; do odoc html-generate $i -o html; done
  File "img.jpg":
  Warning: asset is missing.

Note that the html was generated despite the missing asset (there might be dead refs!)

  $ find html -type f | sort
  html/index/Test/index.html
  html/index/index.html

Which matches the output of the targets command (which emits no warning):

  $ odoc html-targets page-index.odocl -o html
  html/index/index.html

Trying to pass an asset which doesn't exist:
(also: some sed magic due to cmdliner output changing based on the version)

  $ odoc html-generate page-index.odocl --asset img.jpg -o html 2>&1 | \
  > sed 's/…/.../' | sed "s/\`/'/g"
  odoc: option '--asset': no 'img.jpg' file or directory
  Usage: odoc html-generate [OPTION]... FILE.odocl
  Try 'odoc html-generate --help' or 'odoc --help' for more information.

Creating then passing the asset alongside an incorrect one:

  $ touch img.jpg
  $ odoc html-generate page-index.odocl --asset img.jpg --asset test.mli -o html
  File "test.mli":
  Warning: this asset was not declared as a child of index

This time, the asset should have been copied at the right place:

  $ find html -type f | sort
  html/index/Test/index.html
  html/index/img.jpg
  html/index/index.html

Which once again matches the output of the targets command (still no warning!):

  $ odoc html-targets page-index.odocl --asset img.jpg --asset test.mli -o html
  html/index/index.html
  html/index/img.jpg

Let's make sure the manpage and latex renderers "work" too

  $ for i in *.odocl; do odoc man-generate $i -o man; odoc latex-generate $i -o latex; done

  $ find man -type f | sort
  man/index.3o
  man/index/Test.3o

  $ find latex -type f | sort
  latex/index.tex
  latex/index/Test.tex

Notice that the assets are *not* there. This should probably be fixed for the latex backend.
