  $ ocamlc -bin-annot main.ml
  $ odoc compile main.cmt
  $ odoc link main.odoc
  File "main.ml", line 19, characters 5-23:
  Warning: Failed to resolve reference unresolvedroot(`On) Couldn't find "`On"
  File "main.ml", line 18, characters 5-22:
  Warning: Failed to resolve reference unresolvedroot(On) Couldn't find "On"
  File "main.ml", line 17, characters 5-11:
  Warning: Failed to resolve reference unresolvedroot(`On) Couldn't find "`On"
  File "main.ml", line 16, characters 5-10:
  Warning: Failed to resolve reference unresolvedroot(On) Couldn't find "On"

  $ odoc html-generate -o html --indent main.odocl
  $ cat html/Main/index.html | grep "<li>" -A 3
     <ul><li><a href="#type-switch.On"><code>switch.`On</code></a></li>
      <li><a href="#type-switch.Off"><code>switch.`Off</code></a></li>
      <li><a href="#type-switch.On"><code>switch.`On</code></a></li>
      <li><a href="#type-switch.Off"><code>switch.`Off</code></a></li>
      <li><a href="#type-switch.On"><code>switch.`On</code></a></li>
      <li><a href="#type-switch.Off"><code>switch.`Off</code></a></li>
      <li><a href="#type-switch.On"><code>switch.`On</code></a></li>
      <li><a href="#type-switch.Off"><code>switch.`Off</code></a></li>
     </ul><p>References in the environment don't work:</p>
     <ul><li><code>On</code></li><li><code>`On</code></li>
      <li><code>On</code></li><li><code>`On</code></li>
     </ul>
    </div>
   </body>

