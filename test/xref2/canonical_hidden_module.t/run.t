This test explores the interaction of a few different mechanisms of hiding
modules and canonical annotations.

  $ cat test.mli
  module A_nonhidden : sig
    (** @canonical Test.A *)
  
    type t
  end
  
  module B__hidden : sig
    type t
  end
  
  module C__hidden : sig
    (** @canonical Test.C *)
  
    type t
  end
  
  (**/**)
  
  module D_hidden : sig
    (** @canonical Test.D *)
  
    type t
  end
  
  (**/**)
  
  (** This should not have an expansion *)
  module A = A_nonhidden
  
  (** This should have an expansion *)
  module B = B__hidden
  
  (** This should have an expansion *)
  module C = C__hidden
  
  (** This also should have an expansion *)
  module D = D_hidden
  
  
  (** This should render as A.t but link to A_nonhidden/index.html - since A has no expansion *)
  type a = A_nonhidden.t
  
  (** This should have no RHS as it's hidden and there is no canonical alternative *)
  type b = B__hidden.t
  
  (** This should render as C.t and link to C/index.html *)
  type c = C__hidden.t
  
  (** This should render as D.t and link to D/index.html *)
  type d = D_hidden.t
  

See the comments on the types at the end of test.mli for the expectation.

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc link test.odoc
  $ odoc html-generate test.odocl --indent -o .
  $ odoc support-files -o .
  $ find Test -type f | sort
  Test/A/index.html
  Test/A_nonhidden/index.html
  Test/B/index.html
  Test/C/index.html
  Test/D/index.html
  Test/index.html
  $ cat Test/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>Test (Test)</title><link rel="stylesheet" href="../odoc.css"/>
    <meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <header class="odoc-preamble">
     <h1>Module <code><span>Test</span></code></h1>
    </header>
    <div class="odoc-content">
     <div class="odoc-spec">
      <div class="spec module anchored" id="module-A_nonhidden">
       <a href="#module-A_nonhidden" class="anchor"></a>
       <code>
        <span><span class="keyword">module</span> 
         <a href="A_nonhidden/index.html">A_nonhidden</a>
        </span>
        <span> : <span class="keyword">sig</span> ... 
         <span class="keyword">end</span>
        </span>
       </code>
      </div>
     </div>
     <div class="odoc-spec">
      <div class="spec module anchored" id="module-A">
       <a href="#module-A" class="anchor"></a>
       <code>
        <span><span class="keyword">module</span> <a href="A/index.html">A</a>
        </span>
        <span> : <span class="keyword">sig</span> ... 
         <span class="keyword">end</span>
        </span>
       </code>
      </div>
      <div class="spec-doc"><p>This should not have an expansion</p></div>
     </div>
     <div class="odoc-spec">
      <div class="spec module anchored" id="module-B">
       <a href="#module-B" class="anchor"></a>
       <code>
        <span><span class="keyword">module</span> <a href="B/index.html">B</a>
        </span>
        <span> : <span class="keyword">sig</span> ... 
         <span class="keyword">end</span>
        </span>
       </code>
      </div><div class="spec-doc"><p>This should have an expansion</p></div>
     </div>
     <div class="odoc-spec">
      <div class="spec module anchored" id="module-C">
       <a href="#module-C" class="anchor"></a>
       <code>
        <span><span class="keyword">module</span> <a href="C/index.html">C</a>
        </span>
        <span> : <span class="keyword">sig</span> ... 
         <span class="keyword">end</span>
        </span>
       </code>
      </div><div class="spec-doc"><p>This should have an expansion</p></div>
     </div>
     <div class="odoc-spec">
      <div class="spec module anchored" id="module-D">
       <a href="#module-D" class="anchor"></a>
       <code>
        <span><span class="keyword">module</span> <a href="D/index.html">D</a>
        </span>
        <span> : <span class="keyword">sig</span> ... 
         <span class="keyword">end</span>
        </span>
       </code>
      </div>
      <div class="spec-doc"><p>This also should have an expansion</p></div>
     </div>
     <div class="odoc-spec">
      <div class="spec type anchored" id="type-a">
       <a href="#type-a" class="anchor"></a>
       <code><span><span class="keyword">type</span> a</span>
        <span> = <a href="A/index.html#type-t">A.t</a></span>
       </code>
      </div>
      <div class="spec-doc">
       <p>This should render as A.t but link to A_nonhidden/index.html 
        - since A has no expansion
       </p>
      </div>
     </div>
     <div class="odoc-spec">
      <div class="spec type anchored" id="type-b">
       <a href="#type-b" class="anchor"></a>
       <code><span><span class="keyword">type</span> b</span></code>
      </div>
      <div class="spec-doc">
       <p>This should have no RHS as it's hidden and there is no canonical
         alternative
       </p>
      </div>
     </div>
     <div class="odoc-spec">
      <div class="spec type anchored" id="type-c">
       <a href="#type-c" class="anchor"></a>
       <code><span><span class="keyword">type</span> c</span>
        <span> = <a href="C/index.html#type-t">C.t</a></span>
       </code>
      </div>
      <div class="spec-doc">
       <p>This should render as C.t and link to C/index.html</p>
      </div>
     </div>
     <div class="odoc-spec">
      <div class="spec type anchored" id="type-d">
       <a href="#type-d" class="anchor"></a>
       <code><span><span class="keyword">type</span> d</span>
        <span> = <a href="D/index.html#type-t">D.t</a></span>
       </code>
      </div>
      <div class="spec-doc">
       <p>This should render as D.t and link to D/index.html</p>
      </div>
     </div>
    </div>
   </body>
  </html>

