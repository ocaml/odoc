  $ odoc compile --parent-id pkg/doc --output-dir _odoc index.mld
  $ odoc compile --parent-id pkg/doc --output-dir _odoc content.mld
  $ odoc compile --parent-id pkg/doc/dir1 --output-dir _odoc dir1/index.mld
  $ odoc compile --parent-id pkg/doc/dir1 --output-dir _odoc dir1/content_in_dir.mld

  $ odoc link _odoc/pkg/doc/page-index.odoc
  $ odoc link _odoc/pkg/doc/page-content.odoc
  $ odoc link _odoc/pkg/doc/dir1/page-index.odoc
  $ odoc link _odoc/pkg/doc/dir1/page-content_in_dir.odoc

  $ odoc compile-index -P test:_odoc/pkg/doc

  $ ls
  _odoc
  content.mld
  dir1
  index.mld
  index.odoc-index

  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/page-index.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/page-content.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/dir1/page-index.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/dir1/page-content_in_dir.odocl
  $ odoc support-files -o _html

  $ odoc_print _odoc/pkg/doc/page-index.odocl
  {
    "name": {
      "`LeafPage": [
        {
          "Some": {
            "`Page": [ { "Some": { "`Page": [ "None", "pkg" ] } }, "doc" ]
          }
        },
        "index"
      ]
    },
    "root": "<root>",
    "frontmatter": {
      "children": {
        "Some": [ { "Page": "" }, { "Page": "content" }, { "Dir": "dir1" } ]
      }
    },
    "content": [
      {
        "`Heading": [
          { "heading_level": "`Title", "heading_label_explicit": "false" },
          {
            "`Label": [
              {
                "`LeafPage": [
                  {
                    "Some": {
                      "`Page": [
                        { "Some": { "`Page": [ "None", "pkg" ] } }, "doc"
                      ]
                    }
                  },
                  "index"
                ]
              },
              "this-index-has-a-name"
            ]
          },
          [
            { "`Word": "This" },
            "`Space",
            { "`Word": "index" },
            "`Space",
            { "`Word": "has" },
            "`Space",
            { "`Word": "a" },
            "`Space",
            { "`Word": "name" }
          ]
        ]
      },
      { "`Paragraph": [ { "`Word": "Hello" } ] }
    ],
    "digest": "<digest>"
  }


  $ cp -r _html /tmp/html
