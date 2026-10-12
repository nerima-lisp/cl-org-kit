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
(:org-data (:begin 0 :end 713 :contents-begin 0 :contents-end 713)
  (:section (:begin 0 :end 48 :contents-begin 0 :contents-end 48)
    (:keyword (:begin 0 :end 48 :contents-begin nil :contents-end nil :key "TITLE" :value "Element boundaries and empty contents" :options nil))
  )
  (:headline (:begin 48 :end 100 :contents-begin 73 :contents-end 100 :title ("Heading with a section") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
    (:section (:begin 73 :end 100 :contents-begin 73 :contents-end 100)
      (:paragraph (:begin 73 :end 100 :contents-begin 73 :contents-end 100)
        "This section is non-empty.
"
      )
    )
  )
  (:headline (:begin 100 :end 410 :contents-begin 126 :contents-end 410 :title ("Heading with no section") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
    (:headline (:begin 126 :end 410 :contents-begin 159 :contents-end 410 :title ("Its child starts immediately.") :level 2 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
      (:section (:begin 159 :end 410 :contents-begin 159 :contents-end 410)
        (:paragraph (:begin 159 :end 218 :contents-begin 159 :contents-end 217)
          "*No-space heading candidate stays in the section as text.
"
        )
        (:quote-block (:begin 218 :end 244 :contents-begin nil :contents-end nil :type nil))
        (:center-block (:begin 244 :end 272 :contents-begin nil :contents-end nil :type nil))
        (:special-block (:begin 272 :end 297 :contents-begin nil :contents-end nil :type "note" :parameters nil :name nil))
        (:drawer (:begin 297 :end 312 :contents-begin nil :contents-end nil :drawer-name "EMPTY"))
        (:dynamic-block (:begin 312 :end 343 :contents-begin nil :contents-end nil :block-name "empty-dynamic" :arguments nil))
        (:footnote-definition (:begin 343 :end 354 :contents-begin nil :contents-end nil :label "empty"))
        (:footnote-definition (:begin 354 :end 410 :contents-begin 364 :contents-end 409 :label "next")
          (:paragraph (:begin 364 :end 409 :contents-begin 364 :contents-end 409)
            "A following definition closes the empty one.
"
          )
        )
      )
    )
  )
  (:headline (:begin 410 :end 624 :contents-begin 434 :contents-end 624 :title ("Empty property drawer") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
    (:section (:begin 434 :end 624 :contents-begin 434 :contents-end 624)
      (:property-drawer (:begin 434 :end 454 :contents-begin nil :contents-end nil))
      (:plain-list (:begin 454 :end 485 :contents-begin 454 :contents-end 484 :type :unordered)
        (:item (:begin 454 :end 457 :contents-begin nil :contents-end nil :bullet "- " :checkbox nil :counter nil :tag nil))
        (:item (:begin 457 :end 484 :contents-begin 459 :contents-end 484 :bullet "- " :checkbox nil :counter nil :tag nil)
          (:paragraph (:begin 459 :end 484 :contents-begin 459 :contents-end 484)
            "Next list item has text.
"
          )
        )
      )
      (:table (:begin 485 :end 533 :contents-begin 485 :contents-end 533 :type :org :tblfm nil)
        (:table-row (:begin 485 :end 492 :contents-begin 486 :contents-end 491 :type :standard)
          (:table-cell (:begin 486 :end 489 :contents-begin 488 :contents-end 488 :value nil))
          (:table-cell (:begin 489 :end 491 :contents-begin 490 :contents-end 490 :value nil))
        )
        (:table-row (:begin 492 :end 500 :contents-begin nil :contents-end nil :type :rule))
        (:table-row (:begin 500 :end 533 :contents-begin 501 :contents-end 532 :type :standard)
          (:table-cell (:begin 501 :end 532 :contents-begin 502 :contents-end 532 :value nil)
            "final cell has no trailing bar"
          )
        )
      )
      (:paragraph (:begin 533 :end 568 :contents-begin 533 :contents-end 567)
        "This paragraph follows the table.
"
      )
      (:clock (:begin 568 :end 583 :contents-begin nil :contents-end nil :value nil :status :closed))
      (:diary-sexp (:begin 583 :end 588 :contents-begin nil :contents-end nil :value "%%()"))
      (:comment (:begin 588 :end 590 :contents-begin nil :contents-end nil :value ""))
      (:fixed-width (:begin 590 :end 593 :contents-begin nil :contents-end nil :value ""))
      (:paragraph (:begin 593 :end 598 :contents-begin 593 :contents-end 598)
        "----
"
      )
      (:keyword (:begin 598 :end 607 :contents-begin nil :contents-end nil :key "EMPTY" :value "" :options nil))
      (:paragraph (:begin 607 :end 624 :contents-begin 607 :contents-end 623)
        "#+MISSING-COLON
"
      )
    )
  )
  (:headline (:begin 624 :end 713 :contents-begin 656 :contents-end 713 :title ("Planning belongs to a heading") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled (:timestamp (:begin 667 :end 683 :contents-begin nil :contents-end nil :type :active :year-start 2026 :month-start 10 :day-start 12 :hour-start nil :minute-start nil :year-end 2026 :month-end 10 :day-end 12 :hour-end nil :minute-end nil :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil)) :deadline nil :closed nil)
    (:section (:begin 656 :end 713 :contents-begin 656 :contents-end 713)
      (:planning (:begin 656 :end 685 :contents-begin nil :contents-end nil :closed nil :deadline nil :scheduled (:timestamp (:begin 667 :end 683 :contents-begin nil :contents-end nil :type :active :year-start 2026 :month-start 10 :day-start 12 :hour-start nil :minute-start nil :year-end 2026 :month-end 10 :day-end 12 :hour-end nil :minute-end nil :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil))))
      (:latex-environment (:begin 685 :end 713 :contents-begin nil :contents-end nil :value "\\begin{align*}
\\end{align*}
"))
    )
  )
)
