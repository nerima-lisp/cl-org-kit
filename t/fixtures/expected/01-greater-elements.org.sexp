; Generated from org-element-parse-buffer with Emacs 31.1 and Org 9.8.10.
; Offsets are zero-based decoded character positions; :end is exclusive.
; A leading U+FEFF is removed before parsing and excluded from offsets.
; CR and LF each count as one character, so a CRLF pair counts as two.
; org-inlinetask is loaded explicitly with its default minimum level 15.
; Plain text is an unpropertized string.
; Excluded: :parent (cycles/buffer references), :standard-properties (internal markers/buffer),
; :pre-blank and :post-blank (layout counts), :path except on links (file context),
; :CATEGORY and unselected properties such as :raw-link/:raw-value (source spellings);
; text properties are stripped.
(:org-data (:begin 0 :end 1329 :contents-begin 0 :contents-end 1329)
  (:section (:begin 0 :end 592 :contents-begin 0 :contents-end 592)
    (:keyword (:begin 0 :end 27 :contents-begin nil :contents-end nil :key "TITLE" :value "Greater elements" :options nil))
    (:quote-block (:begin 27 :end 90 :contents-begin 41 :contents-end 77 :type nil)
      (:paragraph (:begin 41 :end 77 :contents-begin 41 :contents-end 77)
        "A quote block contains a paragraph.
"
      )
    )
    (:center-block (:begin 90 :end 134 :contents-begin 105 :contents-end 120 :type nil)
      (:paragraph (:begin 105 :end 120 :contents-begin 105 :contents-end 120)
        "Centered text.
"
      )
    )
    (:special-block (:begin 134 :end 218 :contents-begin 167 :contents-end 206 :type "note" :parameters "Optional parameters" :name nil)
      (:paragraph (:begin 167 :end 206 :contents-begin 167 :contents-end 206)
        "A named special block with parameters.
"
      )
    )
    (:drawer (:begin 218 :end 346 :contents-begin 226 :contents-end 339 :drawer-name "NOTES")
      (:paragraph (:begin 226 :end 263 :contents-begin 226 :contents-end 263)
        "A drawer contains ordinary elements.
"
      )
      (:example-block (:begin 263 :end 339 :contents-begin nil :contents-end nil :switches nil :value "The nested example block is a lesser element.
"))
    )
    (:dynamic-block (:begin 346 :end 449 :contents-begin 390 :contents-end 441 :block-name "clocktable" :arguments ":scope file :maxlevel 2")
      (:paragraph (:begin 390 :end 441 :contents-begin 390 :contents-end 441)
        "Dynamic block contents are parsed as Org elements.
"
      )
    )
    (:footnote-definition (:begin 449 :end 492 :contents-begin 460 :contents-end 492 :label "short")
      (:paragraph (:begin 460 :end 492 :contents-begin 460 :contents-end 492)
        "A one-line footnote definition.
"
      )
    )
    (:footnote-definition (:begin 492 :end 592 :contents-begin 502 :contents-end 591 :label "long")
      (:paragraph (:begin 502 :end 544 :contents-begin 502 :contents-end 543)
        "A longer definition with two paragraphs.
"
      )
      (:paragraph (:begin 544 :end 591 :contents-begin 544 :contents-end 591)
        "The second paragraph belongs to this footnote.
"
      )
    )
  )
  (:headline (:begin 592 :end 775 :contents-begin 623 :contents-end 775 :title ("Property drawer and planning") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled (:timestamp (:begin 634 :end 656 :contents-begin nil :contents-end nil :type :active :year-start 2026 :month-start 10 :day-start 12 :hour-start 9 :minute-start 30 :year-end 2026 :month-end 10 :day-end 12 :hour-end 9 :minute-end 30 :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil)) :deadline nil :closed nil)
    (:section (:begin 623 :end 775 :contents-begin 623 :contents-end 775)
      (:planning (:begin 623 :end 657 :contents-begin nil :contents-end nil :closed nil :deadline nil :scheduled (:timestamp (:begin 634 :end 656 :contents-begin nil :contents-end nil :type :active :year-start 2026 :month-start 10 :day-start 12 :hour-start 9 :minute-start 30 :year-end 2026 :month-end 10 :day-end 12 :hour-end 9 :minute-end 30 :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil))))
      (:property-drawer (:begin 657 :end 727 :contents-begin 670 :contents-end 721)
        (:node-property (:begin 670 :end 699 :contents-begin nil :contents-end nil :key "CUSTOM_ID" :value "property-example"))
        (:node-property (:begin 699 :end 721 :contents-begin nil :contents-end nil :key "OWNER+" :value "Japanese 日本語"))
      )
      (:paragraph (:begin 727 :end 775 :contents-begin 727 :contents-end 774)
        "The property drawer follows the planning line.
"
      )
    )
  )
  (:headline (:begin 775 :end 1329 :contents-begin 794 :contents-end 1329 :title ("Lists and tables") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
    (:section (:begin 794 :end 1162 :contents-begin 794 :contents-end 1162)
      (:plain-list (:begin 794 :end 1009 :contents-begin 794 :contents-end 1008 :type :unordered)
        (:item (:begin 794 :end 892 :contents-begin 796 :contents-end 892 :bullet "- " :checkbox nil :counter nil :tag nil)
          (:paragraph (:begin 796 :end 866 :contents-begin 796 :contents-end 866)
            "Unchecked task [ ] with an item body
  Continuation of the same item.
"
          )
          (:plain-list (:begin 866 :end 892 :contents-begin 866 :contents-end 892 :type :unordered)
            (:item (:begin 866 :end 892 :contents-begin 870 :contents-end 892 :bullet "- " :checkbox nil :counter nil :tag nil)
              (:paragraph (:begin 870 :end 892 :contents-begin 870 :contents-end 892)
                "Nested unordered item
"
              )
            )
          )
        )
        (:item (:begin 892 :end 911 :contents-begin 894 :contents-end 911 :bullet "- " :checkbox nil :counter nil :tag nil)
          (:paragraph (:begin 894 :end 911 :contents-begin 894 :contents-end 911)
            "Checked task [X]
"
          )
        )
        (:item (:begin 911 :end 950 :contents-begin 919 :contents-end 950 :bullet "3. " :checkbox nil :counter 3 :tag nil)
          (:paragraph (:begin 919 :end 950 :contents-begin 919 :contents-end 950)
            "Ordered item starting at three
"
          )
        )
        (:item (:begin 950 :end 974 :contents-begin 953 :contents-end 974 :bullet "4. " :checkbox nil :counter nil :tag nil)
          (:paragraph (:begin 953 :end 974 :contents-begin 953 :contents-end 974)
            "Another ordered item
"
          )
        )
        (:item (:begin 974 :end 1008 :contents-begin 984 :contents-end 1008 :bullet "+ " :checkbox nil :counter nil :tag ("Term"))
          (:paragraph (:begin 984 :end 1008 :contents-begin 984 :contents-end 1008)
            "A descriptive list item
"
          )
        )
      )
      (:table (:begin 1009 :end 1086 :contents-begin 1009 :contents-end 1085 :type :org :tblfm nil)
        (:table-row (:begin 1009 :end 1026 :contents-begin 1010 :contents-end 1025 :type :standard)
          (:table-cell (:begin 1010 :end 1017 :contents-begin 1011 :contents-end 1015 :value nil)
            "Name"
          )
          (:table-cell (:begin 1017 :end 1025 :contents-begin 1018 :contents-end 1023 :value nil)
            "Value"
          )
        )
        (:table-row (:begin 1026 :end 1043 :contents-begin nil :contents-end nil :type :rule))
        (:table-row (:begin 1043 :end 1062 :contents-begin 1044 :contents-end 1061 :type :standard)
          (:table-cell (:begin 1044 :end 1052 :contents-begin 1045 :contents-end 1050 :value nil)
            "alpha"
          )
          (:table-cell (:begin 1052 :end 1061 :contents-begin 1053 :contents-end 1059 :value nil)
            (:bold (:begin 1053 :end 1059 :contents-begin 1054 :contents-end 1058)
              "bold"
            )
          )
        )
        (:table-row (:begin 1062 :end 1085 :contents-begin 1063 :contents-end 1084 :type :standard)
          (:table-cell (:begin 1063 :end 1071 :contents-begin 1064 :contents-end 1069 :value nil)
            "omega"
          )
          (:table-cell (:begin 1071 :end 1084 :contents-begin 1072 :contents-end 1082 :value nil)
            "final cell"
          )
        )
      )
      (:table (:begin 1086 :end 1162 :contents-begin nil :contents-end nil :type :table.el :tblfm nil))
    )
    (:headline (:begin 1162 :end 1329 :contents-begin 1235 :contents-end 1329 :title ("Fourteen stars remain below the default inline-task level") :level 14 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
      (:section (:begin 1235 :end 1329 :contents-begin 1235 :contents-end 1329)
        (:inlinetask (:begin 1235 :end 1329 :contents-begin 1268 :contents-end 1309 :title ("Inline task") :level 15 :todo-keyword "TODO" :todo-type :todo :priority nil :tags nil :commentedp nil :archivedp nil)
          (:paragraph (:begin 1268 :end 1309 :contents-begin 1268 :contents-end 1309)
            "The inline task may contain a paragraph.
"
          )
        )
      )
    )
  )
)
