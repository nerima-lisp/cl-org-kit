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
(:org-data (:begin 0 :end 1114 :contents-begin 0 :contents-end 1114)
  (:section (:begin 0 :end 904 :contents-begin 0 :contents-end 904)
    (:keyword (:begin 0 :end 31 :contents-begin nil :contents-end nil :key "TITLE" :value "Lesser element forms" :options nil))
    (:babel-call (:begin 31 :end 46 :contents-begin nil :contents-end nil :call "foo" :inside-header nil :arguments nil :end-header nil))
    (:comment-block (:begin 46 :end 114 :contents-begin nil :contents-end nil :value "Comment block body is kept verbatim.
"))
    (:example-block (:begin 114 :end 176 :contents-begin nil :contents-end nil :switches nil :value "Example block body is literal.
"))
    (:export-block (:begin 176 :end 236 :contents-begin nil :contents-end nil :type "HTML" :value "<p>Export block body.</p>
"))
    (:src-block (:begin 236 :end 300 :contents-begin nil :contents-end nil :language "emacs-lisp" :value "(message \"source block\")
" :switches "-n 1" :parameters nil :name nil))
    (:verse-block (:begin 300 :end 376 :contents-begin 314 :contents-end 363 :type nil)
      "Verse line one
  Verse line two with indentation
"
    )
    (:comment (:begin 376 :end 402 :contents-begin nil :contents-end nil :value "A regular comment line."))
    (:special-block (:begin 402 :end 508 :contents-begin 422 :contents-end 489 :type "nonexistent" :parameters nil :name nil)
      (:paragraph (:begin 422 :end 489 :contents-begin 422 :contents-end 489)
        "Unknown block names are greater special blocks, not lesser blocks.
"
      )
    )
    (:clock (:begin 508 :end 571 :contents-begin nil :contents-end nil :value (:timestamp (:begin 515 :end 562 :contents-begin nil :contents-end nil :type :inactive-range :year-start 2026 :month-start 10 :day-start 12 :hour-start 9 :minute-start 0 :year-end 2026 :month-end 10 :day-end 12 :hour-end 9 :minute-end 42 :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil)) :status :closed))
    (:clock (:begin 571 :end 595 :contents-begin nil :contents-end nil :value (:timestamp (:begin 578 :end 594 :contents-begin nil :contents-end nil :type :inactive :year-start 2026 :month-start 10 :day-start 11 :hour-start nil :minute-start nil :year-end 2026 :month-end 10 :day-end 11 :hour-end nil :minute-end nil :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil)) :status :running))
    (:clock (:begin 595 :end 612 :contents-begin nil :contents-end nil :value nil :status :closed))
    (:diary-sexp (:begin 612 :end 638 :contents-begin nil :contents-end nil :value "%%(org-calendar-holiday)"))
    (:horizontal-rule (:begin 638 :end 644 :contents-begin nil :contents-end nil))
    (:fixed-width (:begin 644 :end 695 :contents-begin nil :contents-end nil :value "A fixed-width line.
Another fixed-width line."))
    (:keyword (:begin 695 :end 724 :contents-begin nil :contents-end nil :key "AUTHOR" :value "Org parser fixture" :options nil))
    (:paragraph (:begin 724 :end 844 :contents-begin 798 :contents-end 843)
      (:link (:begin 798 :end 842 :contents-begin 823 :contents-end 840 :type "https" :path "affiliated-path" :format :bracket)
        "affiliated target"
      )
      "
"
    )
    (:latex-environment (:begin 844 :end 904 :contents-begin nil :contents-end nil :value "\\begin{align*}
2x - 5y &= 8 \\\\
3x + 9y &= -12
\\end{align*}
"))
  )
  (:headline (:begin 904 :end 1114 :contents-begin 935 :contents-end 1114 :title ("Planning and node properties") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline (:timestamp (:begin 945 :end 965 :contents-begin nil :contents-end nil :type :active :year-start 2026 :month-start 10 :day-start 13 :hour-start nil :minute-start nil :year-end 2026 :month-end 10 :day-end 13 :hour-end nil :minute-end nil :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type :all :warning-value 2 :warning-unit :day)) :closed nil)
    (:section (:begin 935 :end 1114 :contents-begin 935 :contents-end 1114)
      (:planning (:begin 935 :end 966 :contents-begin nil :contents-end nil :closed nil :deadline (:timestamp (:begin 945 :end 965 :contents-begin nil :contents-end nil :type :active :year-start 2026 :month-start 10 :day-start 13 :hour-start nil :minute-start nil :year-end 2026 :month-end 10 :day-end 13 :hour-end nil :minute-end nil :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type :all :warning-value 2 :warning-unit :day)) :scheduled nil))
      (:paragraph (:begin 966 :end 997 :contents-begin 966 :contents-end 997)
        "CLOSED: "
        (:timestamp (:begin 974 :end 996 :contents-begin nil :contents-end nil :type :inactive :year-start 2026 :month-start 10 :day-start 10 :hour-start 15 :minute-start 20 :year-end 2026 :month-end 10 :day-end 10 :hour-end 15 :minute-end 20 :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil))
        "
"
      )
      (:drawer (:begin 997 :end 1071 :contents-begin 1010 :contents-end 1065 :drawer-name "PROPERTIES")
        (:paragraph (:begin 1010 :end 1065 :contents-begin 1010 :contents-end 1065)
          ":ID: node-property-value
:EMPTY:
:APPEND+: extra value
"
        )
      )
      (:paragraph (:begin 1071 :end 1114 :contents-begin 1071 :contents-end 1114)
        "A paragraph is the default lesser element.
"
      )
    )
  )
)
