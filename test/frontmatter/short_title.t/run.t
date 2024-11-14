Normal use

  $ cat << EOF > index.mld
  > @short_title First try
  > {0 Test1}
  > EOF
  $ odoc compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc_print _odoc/pkg/page-index.odoc | jq .frontmatter.short_title -c
  {"Some":[{"`Word":"First"},"`Space",{"`Word":"try"}]}

With inline content

  $ cat << EOF > index.mld
  > @short_title with [code] and {e emphasized} content
  > {0 Test1}
  > EOF
  $ odoc compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc_print _odoc/pkg/page-index.odoc | jq .frontmatter.short_title -c
  {"Some":[{"`Word":"with"},"`Space",{"`Code_span":"code"},"`Space",{"`Word":"and"},"`Space",{"`Styled":["`Emphasis",[{"`Word":"emphasized"}]]},"`Space",{"`Word":"content"}]}

With reference or link

  $ cat << EOF > index.mld
  > @short_title with {:link} and {!ref}
  > {0 Test1}
  > EOF
  $ odoc compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc_print _odoc/pkg/page-index.odoc | jq .frontmatter.short_title -c
  {"Some":[{"`Word":"with"},"`Space","`Space",{"`Word":"and"},"`Space"]}

With other block

  $ cat << EOF > index.mld
  > @short_title {[code block]}
  > {0 Test1}
  > EOF
  $ odoc compile --parent-id pkg --output-dir _odoc index.mld
  File "index.mld", line 1, characters 0-27:
  Warning: Short titles cannot contain other block than a single paragraph
  $ odoc_print _odoc/pkg/page-index.odoc | jq .frontmatter.short_title -c
  "None"

  $ cat << EOF > index.mld
  > @short_title paragraph
  > {ul {li yo}}
  > {0 Test1}
  > EOF
  $ odoc compile --parent-id pkg --output-dir _odoc index.mld
  File "index.mld", line 1, character 0 to line 2, character 12:
  Warning: Short titles cannot contain other block than a single paragraph
  $ odoc_print _odoc/pkg/page-index.odoc | jq .frontmatter.short_title -c
  "None"

Multiple occurrence

  $ cat << EOF > index.mld
  > @short_title yay
  > @short_title yo
  > {0 Test1}
  > EOF
  $ odoc compile --parent-id pkg --output-dir _odoc index.mld
  File "index.mld", line 2, characters 0-15:
  Warning: Duplicated @short_title entry
  $ odoc_print _odoc/pkg/page-index.odoc | jq .frontmatter.short_title -c
  {"Some":[{"`Word":"yay"}]}
