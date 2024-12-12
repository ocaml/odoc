When there is no frontmatter, everything is normal

  $ odoc compile zero_frontmatter.mld
  $ odoc_print page-zero_frontmatter.odoc | jq '.frontmatter'
  {
    "children": "None",
    "short_title": "None",
    "toc_status": "None",
    "order_category": "None"
  }

When there is one frontmatter, it is extracted from the content:

  $ odoc compile one_frontmatter.mld
  File "one_frontmatter.mld":
  Warning: Non-index page cannot specify @children_order.
  $ odoc_print page-one_frontmatter.odoc | jq '.frontmatter'
  {
    "children": {
      "Some": [
        {
          "Page": "page1"
        },
        {
          "Page": "page2"
        }
      ]
    },
    "short_title": {
      "Some": [
        {
          "`Word": "yes!"
        }
      ]
    },
    "toc_status": "None",
    "order_category": "None"
  }
  $ odoc_print page-one_frontmatter.odoc | jq '.content.elements'
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
            "one-frontmatter"
          ]
        },
        [
          {
            "`Word": "One"
          },
          "`Space",
          {
            "`Word": "frontmatter"
          }
        ]
      ]
    }
  ]

When there is more than one children order, we raise a warning and keep only the first entry:

  $ odoc compile two_frontmatters.mld
  File "two_frontmatters.mld", line 2, characters 0-25:
  Warning: Duplicated @children_order entry
  File "two_frontmatters.mld":
  Warning: Non-index page cannot specify @children_order.
  $ odoc_print page-two_frontmatters.odoc | jq '.frontmatter'
  {
    "children": {
      "Some": [
        {
          "Page": "bli1"
        },
        {
          "Page": "bli2"
        }
      ]
    },
    "short_title": "None",
    "toc_status": "None",
    "order_category": "None"
  }
  $ odoc_print page-two_frontmatters.odoc | jq '.content.elements'
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
            "two-frontmatters"
          ]
        },
        [
          {
            "`Word": "Two"
          },
          "`Space",
          {
            "`Word": "frontmatters"
          }
        ]
      ]
    }
  ]
