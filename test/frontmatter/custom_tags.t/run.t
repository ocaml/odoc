  $ odoc compile custom.mld
  $ odoc html-generate --as-json page-custom.odoc -o .
  $ jq . custom.html.json
  {
    "header": "<section id=\"section-custom-tags\"><h1 id=\"custom-tags\"><a href=\"#custom-tags\" class=\"anchor\"></a>Custom Tags</h1></section>",
    "type": "documentation",
    "uses_katex": false,
    "breadcrumbs": [
      {
        "name": "Index",
        "href": "index.html",
        "kind": "leaf-page"
      },
      {
        "name": "custom",
        "href": "#",
        "kind": "leaf-page"
      }
    ],
    "toc": [],
    "source_anchor": null,
    "preamble": "<ul class=\"at-tags\"><li class=\"hello\"><span class=\"at-tag\">hello</span> <p>world</p></li></ul>",
    "content": "",
    "frontmatter": {
      "other_config": [
        [
          "hello",
          "world"
        ]
      ]
    }
  }

