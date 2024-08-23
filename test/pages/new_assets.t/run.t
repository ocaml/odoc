  $ odoc compile-asset --name img.png --parent-id root/test --output-dir odoc

  $ odoc_print odoc/root/test/asset-img.png.odoc
  {
    "name": {
      "`AssetFile": [
        { "`Page": [ { "Some": { "`Page": [ "None", "root" ] } }, "test" ] },
        "img.png"
      ]
    },
    "root": "<root>"
  }

  $ echo "Hello!" > img.png

  $ odoc html-generate-asset --output-dir _html --asset-unit odoc/root/test/asset-img.png.odoc img.png

  $ find _html -name img.png
  _html/root/test/img.png

  $ cat  $(find _html -name img.png)
  Hello!
