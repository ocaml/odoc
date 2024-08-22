We need to odoc-compile the package mld file, listing its children

  $ odoc compile index.mld --parent-id pkg1/doc/ --output-dir _odoc

  $ odoc compile-asset --parent-id pkg1/doc/ --output-dir _odoc --name caml.gif
  $ odoc compile-asset --parent-id pkg1/doc/ --output-dir _odoc --name Cri_du_chameau.ogg
  $ odoc compile-asset --parent-id pkg1/doc/ --output-dir _odoc --name flower.webm

This will have produced a file called 'page-index.odoc'.

Link (and generate the HTML):
  $ odoc link -P pkg1:_odoc/pkg1/doc _odoc/pkg1/doc/page-index.odoc
  File "index.mld", line 41, characters 48-64:
  Warning: Failed to resolve reference ./module-x Path 'module-x' not found
  File "index.mld", line 12, characters 28-83:
  Warning: Failed to resolve reference ./camezfzeffl.gif Path 'camezfzeffl.gif' not found
  File "index.mld", line 11, characters 31-53:
  Warning: Failed to resolve reference ./caqzdqzdml.gif Path 'caqzdqzdml.gif' not found
  $ odoc html-generate -o html --indent _odoc/pkg1/doc/page-index.odocl
  $ odoc support-files -o html

To test visually, indent:
 $ cp -r html /tmp/
 $ firefox /tmp/html/index/index.html

Testing the working references:

  $ cat html/pkg1/doc/index.html | grep img
        <a href="caml.gif" class="img-link">
         <img src="caml.gif" alt="caml.gif"/>
        <a href="caml.gif" class="img-link">
         <img src="caml.gif" alt="With alt text and {b emphasis}"/>
        <a href="https://picsum.photos/200/300" class="img-link">
         <img src="https://picsum.photos/200/300" alt="reference"/>
        <a href="https://picsum.photos/200/300" class="img-link">
         <img src="https://picsum.photos/200/300"

  $ cat html/pkg1/doc/index.html | grep video
       <li><a href="#video">Video</a>
     </div><h2 id="video"><a href="#video" class="anchor"></a>Video</h2>
     </h3><div><video src="flower.webm" controls="controls"></video></div>
      <video
       src="https://interactive-examples.mdn.mozilla.net/media/cc0-videos/flower.webm"
      </video>

  $ cat html/pkg1/doc/index.html | grep audio
       <li><a href="#audio">Audio</a>
     </ul><h2 id="audio"><a href="#audio" class="anchor"></a>Audio</h2>
     <div><audio src="Cri_du_chameau.ogg" controls="controls"></audio></div>
      <audio
      </audio>

Testing the unresolved references:

  $ cat html/pkg1/doc/index.html | grep xref-unresolved
       <div><span class="xref-unresolved">./caqzdqzdml.gif</span></div>
       <div><span class="xref-unresolved">With alt text and {b emphasis}</span>
       <div><span class="xref-unresolved">./module-x</span></div>

Testing latex and manpages

  $ odoc latex-generate -o latex _odoc/pkg1/doc/page-index.odocl
  $ cat latex/pkg1/doc/index.tex | grep gif
  caml.gif
  ./caqzdqzdml.gif

  $ odoc man-generate -o man _odoc/pkg1/doc/page-index.odocl
  $ cat man/pkg1/doc/index.3o | grep gif
  caml\.gif
  \./caqzdqzdml\.gif
  $ cat man/pkg1/doc/index.3o | grep "With alt text"
  With alt text and {b emphasis}
  With alt text and {b emphasis}
