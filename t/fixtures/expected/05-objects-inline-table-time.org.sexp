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
(:org-data (:begin 0 :end 842 :contents-begin 0 :contents-end 842)
  (:section (:begin 0 :end 842 :contents-begin 0 :contents-end 842)
    (:keyword (:begin 0 :end 53 :contents-begin nil :contents-end nil :key "TITLE" :value "Inline source, table cells, and timestamps" :options nil))
    (:paragraph (:begin 53 :end 220 :contents-begin 53 :contents-end 219)
      "Inline Babel call: "
      (:inline-babel-call (:begin 72 :end 87 :contents-begin nil :contents-end nil :call "sum" :inside-header nil :arguments "1, 2" :end-header nil))
      "and "
      (:inline-babel-call (:begin 91 :end 119 :contents-begin nil :contents-end nil :call "greet" :inside-header "lang=ja" :arguments "\"こんにちは\"" :end-header nil))
      ".
Inline source: "
      (:inline-src-block (:begin 136 :end 173 :contents-begin nil :contents-end nil :language "emacs-lisp" :value "(+ 1 2)" :parameters ":results raw"))
      ".
Inline source boundary: src_ {no language}.
"
    )
    (:paragraph (:begin 220 :end 341 :contents-begin 220 :contents-end 340)
      "Line break follows this text"
      (:line-break (:begin 248 :end 251 :contents-begin nil :contents-end nil))
      "and starts a new line in the same paragraph.
Boundary backslash is ordinary text here: \\
"
    )
    (:paragraph (:begin 341 :end 405 :contents-begin 341 :contents-end 404)
      "Statistics cookies: "
      (:statistics-cookie (:begin 361 :end 367 :contents-begin nil :contents-end nil :value "[2/5]" :type nil))
      "and "
      (:statistics-cookie (:begin 371 :end 376 :contents-begin nil :contents-end nil :value "[40%]" :type nil))
      ".
Boundary cookie: [2 / 5].
"
    )
    (:paragraph (:begin 405 :end 669 :contents-begin 405 :contents-end 668)
      "Active timestamp "
      (:timestamp (:begin 422 :end 453 :contents-begin nil :contents-end nil :type :active :year-start 2026 :month-start 10 :day-start 12 :hour-start 9 :minute-start 30 :year-end 2026 :month-end 10 :day-end 12 :hour-end 9 :minute-end 30 :repeater-type :cumulate :repeater-value 1 :repeater-unit :week :warning-type :all :warning-value 2 :warning-unit :day))
      "and inactive "
      (:timestamp (:begin 466 :end 482 :contents-begin nil :contents-end nil :type :inactive :year-start 2026 :month-start 10 :day-start 11 :hour-start nil :minute-start nil :year-end 2026 :month-end 10 :day-end 11 :hour-end nil :minute-end nil :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil))
      ".
Active range "
      (:timestamp (:begin 497 :end 531 :contents-begin nil :contents-end nil :type :active-range :year-start 2026 :month-start 10 :day-start 12 :hour-start nil :minute-start nil :year-end 2026 :month-end 10 :day-end 14 :hour-end nil :minute-end nil :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil))
      ".
Time range "
      (:timestamp (:begin 544 :end 572 :contents-begin nil :contents-end nil :type :active-range :year-start 2026 :month-start 10 :day-start 12 :hour-start 9 :minute-start 0 :year-end 2026 :month-end 10 :day-end 12 :hour-end 10 :minute-end 30 :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil))
      ".
Diary timestamp "
      (:timestamp (:begin 590 :end 625 :contents-begin nil :contents-end nil :type :diary :year-start nil :month-start nil :day-start nil :hour-start 12 :minute-start 0 :year-end nil :month-end nil :day-end nil :hour-end 14 :minute-end 0 :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil))
      ".
Timestamp boundary "
      (:timestamp (:begin 646 :end 666 :contents-begin nil :contents-end nil :type :active :year-start 2026 :month-start 13 :day-start 99 :hour-start nil :minute-start nil :year-end 2026 :month-end 13 :day-end 99 :hour-end nil :minute-end nil :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil))
      ".
"
    )
    (:table (:begin 669 :end 842 :contents-begin 669 :contents-end 842 :type :org :tblfm nil)
      (:table-row (:begin 669 :end 729 :contents-begin 670 :contents-end 728 :type :standard)
        (:table-cell (:begin 670 :end 689 :contents-begin 671 :contents-end 687 :value nil)
          "Cell with "
          (:bold (:begin 681 :end 687 :contents-begin 682 :contents-end 686)
            "bold"
          )
        )
        (:table-cell (:begin 689 :end 728 :contents-begin 690 :contents-end 726 :value nil)
          "cell with "
          (:link (:begin 700 :end 726 :contents-begin 720 :contents-end 724 :type "https" :path "table-path" :format :bracket)
            "link"
          )
        )
      )
      (:table-row (:begin 729 :end 787 :contents-begin nil :contents-end nil :type :rule))
      (:table-row (:begin 787 :end 842 :contents-begin 788 :contents-end 841 :type :standard)
        (:table-cell (:begin 788 :end 803 :contents-begin 789 :contents-end 801 :value nil)
          "Japanese 日本語"
        )
        (:table-cell (:begin 803 :end 841 :contents-begin 804 :contents-end 830 :value nil)
          "timestamp "
          (:timestamp (:begin 814 :end 830 :contents-begin nil :contents-end nil :type :active :year-start 2026 :month-start 10 :day-start 12 :hour-start nil :minute-start nil :year-end 2026 :month-end 10 :day-end 12 :hour-end nil :minute-end nil :repeater-type nil :repeater-value nil :repeater-unit nil :warning-type nil :warning-value nil :warning-unit nil))
        )
      )
    )
  )
)
