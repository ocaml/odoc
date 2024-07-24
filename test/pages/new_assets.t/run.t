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
