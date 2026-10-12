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
(:org-data (:begin 0 :end 805 :contents-begin 0 :contents-end 805)
  (:section (:begin 0 :end 805 :contents-begin 0 :contents-end 805)
    (:keyword (:begin 0 :end 50 :contents-begin nil :contents-end nil :key "TITLE" :value "Links, citations, footnotes, and macros" :options nil))
    (:paragraph (:begin 50 :end 157 :contents-begin 50 :contents-end 156)
      "Radio target definition: "
      (:radio-target (:begin 75 :end 102 :contents-begin 78 :contents-end 99 :value "radio *target* phrase")
        "radio "
        (:bold (:begin 84 :end 93 :contents-begin 85 :contents-end 91)
          "target"
        )
        "phrase"
      )
      ".
Later radio link occurrence: "
      (:link (:begin 133 :end 154 :contents-begin 133 :contents-end 154 :type "radio" :path "radio *target* phrase" :format :plain)
        "radio "
        (:bold (:begin 139 :end 148 :contents-begin 140 :contents-end 146)
          "target"
        )
        "phrase"
      )
      ".
"
    )
    (:paragraph (:begin 157 :end 396 :contents-begin 157 :contents-end 395)
      "Plain link: "
      (:link (:begin 169 :end 185 :contents-begin nil :contents-end nil :type "https" :path "plain-path" :format :plain))
      ".
Angle link: "
      (:link (:begin 199 :end 217 :contents-begin nil :contents-end nil :type "https" :path "angle-path" :format :angle))
      ".
Regular link with object description: "
      (:link (:begin 257 :end 301 :contents-begin 279 :contents-end 299 :type "https" :path "bracket-path" :format :bracket)
        "a "
        (:bold (:begin 281 :end 288 :contents-begin 282 :contents-end 286)
          "bold"
        )
        "description"
      )
      ".
Regular internal link: "
      (:link (:begin 326 :end 356 :contents-begin 343 :contents-end 354 :type "custom-id" :path "local-anchor" :format :bracket)
        "anchor link"
      )
      ".
Target definition: "
      (:target (:begin 377 :end 393 :contents-begin nil :contents-end nil :value "local-anchor"))
      ".
"
    )
    (:paragraph (:begin 396 :end 739 :contents-begin 396 :contents-end 738)
      "Macro calls: "
      (:macro (:begin 409 :end 421 :contents-begin nil :contents-end nil :key "title" :args nil :value "{{{title}}}"))
      "and "
      (:macro (:begin 425 :end 441 :contents-begin nil :contents-end nil :key "greet" :args ("日本語") :value "{{{greet(日本語)}}}"))
      ".
Export snippet: "
      (:export-snippet (:begin 459 :end 494 :contents-begin nil :contents-end nil :back-end "html" :value "<mark>inline export</mark>"))
      ".
Boundary snippet: @@html:unclosed export snippet.
Footnote references: "
      (:footnote-reference (:begin 567 :end 578 :contents-begin nil :contents-end nil :label "short" :type :standard))
      "and "
      (:footnote-reference (:begin 582 :end 599 :contents-begin 587 :contents-end 598 :label nil :type :inline)
        "inline note"
      )
      ".
Citation with two references: "
      (:citation (:begin 631 :end 679 :contents-begin 644 :contents-end 670 :style "t" :key nil)
        (:citation-reference (:begin 644 :end 655 :contents-begin nil :contents-end nil :key "smith2024" :prefix nil :suffix nil :locator nil))
        (:citation-reference (:begin 655 :end 670 :contents-begin nil :contents-end nil :key "doe-2025" :prefix nil :suffix (" p. 4") :locator nil))
      )
      ".
Citation reference boundary: [cite @not-a-citation-key].
"
    )
    (:footnote-definition (:begin 739 :end 805 :contents-begin 750 :contents-end 805 :label "short")
      (:paragraph (:begin 750 :end 805 :contents-begin 750 :contents-end 805)
        "A footnote definition paired with the reference above.
"
      )
    )
  )
)
