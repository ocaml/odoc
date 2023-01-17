Verify the behavior on functors.

  $ ocamlc -c -o s.cmo s.ml -bin-annot -I .
  $ ocamlc -c -o a.cmo a.ml -bin-annot -I .
  $ ocamlc -c -o b.cmo b.ml -bin-annot -I .
  $ odoc compile --impl s.ml -I . s.cmt
  $ odoc compile --impl a.ml -I . a.cmt
  $ odoc compile --impl b.ml -I . b.cmt
  $ odoc link -I . s.odoc
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc html-generate --indent -o html s.odocl
  $ odoc html-generate --indent -o html a.odocl
  $ odoc html-generate --indent -o html b.odocl

  $ find html | sort
  html
  html/A
  html/A/A.ml.html
  html/A/F
  html/A/F/argument-1-S
  html/A/F/argument-1-S/index.html
  html/A/F/index.html
  html/A/index.html
  html/B
  html/B/B.ml.html
  html/B/R
  html/B/R/index.html
  html/B/S
  html/B/S/index.html
  html/B/index.html
  html/S
  html/S/S.ml.html
  html/S/index.html
  html/S/module-type-S
  html/S/module-type-S/index.html

In this test, the functor expansion contains the right link.
TODO

  $ cat html/A/F/index.html | grep source_link --context=1
  [1]

  $ cat html/A/A.ml.html | grep L3
  </span><span id="L3" class="source_line"></span>  <span class="LET">let</span> <span id="y_272"><span id="def-A2"><span class="LIDENT">y</span></span></span> <span class="EQUAL">=</span> <span class="UIDENT">S</span><span class="DOT">.</span><span class="LIDENT">x</span><span class="EOL">

However, on functor results, there is a link to source in the file:

  $ cat html/B/R/index.html | grep source_link --context=2
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>
       <a href="../../A/A.ml.html#def-A1" class="source_link">Source</a>
       <code><span><span class="keyword">type</span> t</span>
        <span> = <a href="../S/index.html#type-t">S.t</a></span>
  --
      <div class="spec value anchored" id="val-y">
       <a href="#val-y" class="anchor"></a>
       <a href="../../A/A.ml.html#def-A2" class="source_link">Source</a>
       <code>
        <span><span class="keyword">val</span> y : 

On functor parameter, the link is right (modulo the fact that sub-module type
links are all to the whole module type definition):

  $ cat html/A/F/argument-1-S/index.html | grep source_link --context=1
  [1]

  $ cat html/A/F/argument-1-S/../../../S/S.ml.html | grep L1
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>Source: S.ml (S)</title><link rel="stylesheet" href="../odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/></head><body class="odoc-src"><pre><code><span><span id="def-S2"><span class="MODULE"><span id="L1" class="source_line"></span>module</span> <span class="TYPE">type</span> <span class="UIDENT">S</span> <span class="EQUAL">=</span> <span class="SIG">sig</span><span class="EOL">
