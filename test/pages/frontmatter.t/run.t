When there is no frontmatter, everything is normal

  $ odoc compile zero_frontmatter.mld
  $ odoc_print page-zero_frontmatter.odoc | jq '.frontmatter'
  "None"

When there is one frontmatter, it is extracted from the content:

  $ odoc compile one_frontmatter.mld
  $ odoc_print page-one_frontmatter.odoc | jq '.frontmatter'
  {
    "Some": [
      [
        "bli1",
        " bloblobloblo1"
      ],
      [
        "bli2",
        " bloblobloblo2"
      ]
    ]
  }
  $ odoc_print page-one_frontmatter.odoc | jq '.content'
  [
    {
      "`Heading": [
        {
          "heading_level": "`Title",
          "heading_label_explicit": "false"
        },
        {
          "`Label": [
            {
              "`LeafPage": [
                "None",
                "one_frontmatter"
              ]
            },
            "title"
          ]
        },
        [
          {
            "`Word": "Title"
          }
        ]
      ]
    }
  ]

When there is more than one frontmatter, they are all extracted from the content:

  $ odoc compile two_frontmatters.mld
  $ odoc_print page-two_frontmatters.odoc | jq '.frontmatter'
  {
    "Some": [
      [
        "bli1",
        " bloblobloblo1"
      ],
      [
        "bli2",
        " bloblobloblo2"
      ]
    ]
  }
  $ odoc_print page-two_frontmatters.odoc | jq '.content'
  [
    {
      "`Heading": [
        {
          "heading_level": "`Title",
          "heading_label_explicit": "false"
        },
        {
          "`Label": [
            {
              "`LeafPage": [
                "None",
                "two_frontmatters"
              ]
            },
            "title"
          ]
        },
        [
          {
            "`Word": "Title"
          }
        ]
      ]
    },
    {
      "`Code_block": [
        {
          "Some": "frontmatter"
        },
        "bli3: bloblobloblo1"
      ]
    }
  ]
