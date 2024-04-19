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
  File "main.ml", line 13, characters 5-31:
  Warning: Failed to resolve reference unresolvedroot(switch).`Off Couldn't find "`Off"
  File "main.ml", line 12, characters 5-29:
  Warning: Failed to resolve reference unresolvedroot(switch).On Couldn't find "On"
  File "main.ml", line 11, characters 5-19:
  Warning: Failed to resolve reference unresolvedroot(switch).`Off Couldn't find "`Off"
  File "main.ml", line 10, characters 5-17:
  Warning: Failed to resolve reference unresolvedroot(switch).On Couldn't find "On"
  File "main.ml", line 9, characters 5-36:
  Warning: Failed to resolve reference unresolvedroot(switch).`Off Couldn't find "`Off"
  File "main.ml", line 8, characters 5-34:
  Warning: Failed to resolve reference unresolvedroot(switch).On Couldn't find "On"
  File "main.ml", line 7, characters 5-24:
  Warning: Failed to resolve reference unresolvedroot(switch).`Off Couldn't find "`Off"
  File "main.ml", line 6, characters 5-22:
  Warning: Failed to resolve reference unresolvedroot(switch).On Couldn't find "On"

  $ odoc html-generate -o html --indent main.odocl
  $ cat html/Main/index.html | grep "<li>" -A 3
     <ul><li><code>switch.On</code></li><li><code>switch.`Off</code></li>
      <li><code>switch.On</code></li><li><code>switch.`Off</code></li>
      <li><code>switch.On</code></li><li><code>switch.`Off</code></li>
      <li><code>switch.On</code></li><li><code>switch.`Off</code></li>
     </ul><p>References in the environment don't work:</p>
     <ul><li><code>On</code></li><li><code>`On</code></li>
      <li><code>On</code></li><li><code>`On</code></li>
     </ul>
    </div>
   </body>

