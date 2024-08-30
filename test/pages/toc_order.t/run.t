  $ odoc compile --parent-id pkg/doc --output-dir _odoc index.mld
  $ odoc compile --parent-id pkg/doc --output-dir _odoc content.mld
  $ odoc compile --parent-id pkg/doc/dir1 --output-dir _odoc dir1/index.mld
  $ odoc compile --parent-id pkg/doc/dir1 --output-dir _odoc dir1/content_in_dir.mld

  $ odoc link _odoc/pkg/doc/page-index.odoc
  $ odoc link _odoc/pkg/doc/page-content.odoc
  $ odoc link _odoc/pkg/doc/dir1/page-index.odoc
  $ odoc link _odoc/pkg/doc/dir1/page-content_in_dir.odoc

  $ odoc compile-index -P test:_odoc/pkg/doc

  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/page-index.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/page-content.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/dir1/page-index.odocl
  $ odoc html-generate --index index.odoc-index -o _html  _odoc/pkg/doc/dir1/page-content_in_dir.odocl
  $ odoc support-files -o _html

  $ odoc_print _odoc/pkg/doc/page-index.odocl | jq .frontmatter
  {
    "children": {
      "Some": [
        {
          "Page": "content"
        },
        {
          "Dir": "dir1"
        }
      ]
    }
  }


  $ cp -r _html /tmp/html
