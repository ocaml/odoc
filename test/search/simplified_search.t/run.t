Compile the files

  $ ocamlc -c j.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

Compile and link the documentation

  $ odoc compile -I . page.mld
  $ odoc compile --parent-id page --output-dir . -I . j.cmt
  $ odoc compile --parent-id page --output-dir . -I . main.cmt

  $ odoc link -I . page/j.odoc
  $ odoc link -I . page/main.odoc
  $ odoc link -I . page-page.odoc

  $ odoc compile-index --json -o index.json --simplified-json --wrap-json --root .

  $ cat index.json 
  let documents = [{"name":"index","prefixname":"","kind":"page","url":"/index.html","comment":""},{"name":"page","prefixname":"","kind":"page","url":"/page.html","comment":"A title\u000AA paragraph\u000Asome verbatim\u000Aand code\u000Aa list of things bliblib"},{"name":"page","prefixname":"","kind":"page","url":"/page/index.html","comment":""},{"name":"J","prefixname":"","kind":"module","url":"/page/J/index.html","comment":"a paragraph one"},{"name":"uu","prefixname":"J","kind":"value","url":"/page/J/index.html#val-uu","comment":""},{"name":"J","prefixname":"","kind":"module","url":"/page/J/index.html","comment":"a paragraph two"},{"name":"Main","prefixname":"","kind":"module","url":"/page/Main/index.html","comment":""},{"name":"t","prefixname":"Main","kind":"type","url":"/page/Main/index.html#type-t","comment":"A comment"},{"name":"X","prefixname":"Main","kind":"module","url":"/page/Main/X/index.html","comment":""},{"name":"c","prefixname":"Main.X","kind":"value","url":"/page/Main/X/index.html#val-c","comment":"A value inside a module"},{"name":"tdzdz","prefixname":"Main","kind":"type","url":"/page/Main/index.html#type-tdzdz","comment":"A comment aaaaaaaaaa"},{"name":"A","prefixname":"Main.tdzdz","kind":"constructor","url":"/page/Main/index.html#type-tdzdz.A","comment":""},{"name":"B","prefixname":"Main.tdzdz","kind":"constructor","url":"/page/Main/index.html#type-tdzdz.B","comment":"Bliiiiiiiiiii"},{"name":"Main","prefixname":"","kind":"module","url":"/page/Main/index.html","comment":"this is a title\u000Aand this is a paragraph"},{"name":"M","prefixname":"Main","kind":"module","url":"/page/Main/M/index.html","comment":""},{"name":"t","prefixname":"Main.M","kind":"type","url":"/page/Main/M/index.html#type-t","comment":"dsdsd"},{"name":"v","prefixname":"Main","kind":"value","url":"/page/Main/index.html#val-v","comment":"a reference , and some formatted content with code and\u000A  code blocks"},{"name":"lorem","prefixname":"Main","kind":"value","url":"/page/Main/index.html#val-lorem","comment":"lorem 1 and a link"},{"name":"lorem2","prefixname":"Main","kind":"value","url":"/page/Main/index.html#val-lorem2","comment":"lorem 2"},{"name":"lorem3","prefixname":"Main","kind":"value","url":"/page/Main/index.html#val-lorem3","comment":"lorem 3"},{"name":"lorem4","prefixname":"Main","kind":"value","url":"/page/Main/index.html#val-lorem4","comment":"lorem 4"},{"name":"I","prefixname":"Main","kind":"module","url":"/page/Main/I/index.html","comment":""},{"name":"x","prefixname":"Main.I","kind":"value","url":"/page/Main/I/index.html#val-x","comment":""},{"name":"I","prefixname":"Main","kind":"module","url":"/page/Main/I/index.html","comment":"a paragraph\u000Aand another\u000Averbatim\u000Ax + 1\u000Ablibli"},{"name":"y","prefixname":"Main.I","kind":"value","url":"/page/Main/I/index.html#val-y","comment":""},{"name":"x","prefixname":"Main","kind":"value","url":"/page/Main/index.html#val-x","comment":""},{"name":"Main","prefixname":"","kind":"module","url":"/page/Main/index.html","comment":"a paragraph\u000Aand another\u000Averbatim\u000Ax + 1\u000Ablibli"},{"name":"y","prefixname":"Main","kind":"value","url":"/page/Main/index.html#val-y","comment":""},{"name":"uu","prefixname":"Main","kind":"value","url":"/page/Main/index.html#val-uu","comment":""}];
  const options = { keys: ['name', 'comment'] };
  var idx_fuse = new Fuse(documents, options);


