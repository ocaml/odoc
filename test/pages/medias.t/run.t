We need to odoc-compile the package mld file, listing its children

  $ odoc compile index.mld --child asset-caml.gif
  File "index.mld", line 33, characters 30-38:
  Warning: Non-asset reference is not allowed in media target.
  Suggestion: Use a reference to an asset

This will have produced a file called 'page-index.odoc'.

Link and generate the HTML (forgetting the asset!):

  $ odoc link page-index.odoc
  File "index.mld", line 12, characters 28-85:
  Warning: Failed to resolve reference unresolvedroot(camezfzeffl.gif) Couldn't find asset "camezfzeffl.gif"
  File "index.mld", line 11, characters 31-55:
  Warning: Failed to resolve reference unresolvedroot(caqzdqzdml.gif) Couldn't find asset "caqzdqzdml.gif"
  $ odoc html-generate -o html --indent --asset caml.gif page-index.odocl
  $ odoc support-files -o html

To test visually, indent:
 $ cp -r html /tmp/
 $ firefox /tmp/html/index/index.html

Testing the working references:

  $ cat html/index/index.html | grep img
        <a href="caml.gif" class="img-link">
         <img src="caml.gif" alt="caml.gif"/>
        <a href="caml.gif" class="img-link">
         <img src="caml.gif" alt="With alt text and emphasis"/>
        <a href="https://picsum.photos/200/300" class="img-link">
         <img src="https://picsum.photos/200/300" alt="reference"/>
        <a href="https://picsum.photos/200/300" class="img-link">
         <img src="https://picsum.photos/200/300"

  $ cat html/index/index.html | grep video
      <li><a href="#video">Video</a>
     </div><h2 id="video"><a href="#video" class="anchor"></a>Video</h2>
      <video
       src="https://interactive-examples.mdn.mozilla.net/media/cc0-videos/flower.webm"
      </video>

  $ cat html/index/index.html | grep audio
      <li><a href="#audio">Audio</a>
     </ul><h2 id="audio"><a href="#audio" class="anchor"></a>Audio</h2>
      <audio
      </audio>

Testing the unresolved references:

  $ cat html/index/index.html | grep xref-unresolved
       <div><span class="xref-unresolved"><code>caqzdqzdml.gif</code></span>
        <span class="xref-unresolved">With alt text and <b>emphasis</b></span>

Testing latex and manpages

  $ odoc latex-generate -o latex page-index.odocl
  $ cat latex/index.tex | grep ocamlinlinecode
  \ocamlinlinecode{caml.\allowbreak{}gif}
  \ocamlinlinecode{caqzdqzdml.\allowbreak{}gif}
  \ocamlinlinecode{https://picsum.\allowbreak{}photos/200/300}
  \ocamlinlinecode{https://upload.\allowbreak{}wikimedia.\allowbreak{}org/wikipedia/commons/f/f1/Cri\_\allowbreak{}du\_\allowbreak{}chameau.\allowbreak{}ogg}
  \ocamlinlinecode{https://interactive-examples.\allowbreak{}mdn.\allowbreak{}mozilla.\allowbreak{}net/media/cc0-videos/flower.\allowbreak{}webm}
  \ocamlinlinecode{module-x}

  $ odoc man-generate -o man page-index.odocl
  $ cat man/index.3o | grep gif
  caml\.gif
  caqzdqzdml\.gif
  $ cat man/index.3o | grep "With alt text"
  With alt text and \fBemphasis\fR
  With alt text and \fBemphasis\fR
