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
(:org-data (:begin 0 :end 1128 :contents-begin 0 :contents-end 1128)
  (:section (:begin 0 :end 1128 :contents-begin 0 :contents-end 1128)
    (:keyword (:begin 0 :end 44 :contents-begin nil :contents-end nil :key "TITLE" :value "Object boundaries and near misses" :options nil))
    (:paragraph (:begin 44 :end 264 :contents-begin 44 :contents-end 263)
      "Entity boundary: "
      (:latex-fragment (:begin 61 :end 73 :contents-begin nil :contents-end nil :value "\\notAnEntity"))
      ".
LaTeX boundary: unclosed inline math \\(a+b and \\[c=d.
Export snippet boundary: @@html:<b>unclosed.
Footnote boundaries: [fn:] [fn:has space] [fn:
Citation boundary: [cite @missing-colon].
"
    )
    (:paragraph (:begin 264 :end 423 :contents-begin 264 :contents-end 422)
      "Inline Babel boundary: call_(unclosed arguments.
Inline source boundary: src"
      (:subscript (:begin 340 :end 346 :contents-begin 341 :contents-end 346 :use-brackets-p nil)
        "emacs"
      )
      "-lisp{unclosed body.
Line-break boundary: this has one trailing backslash \\
"
    )
    (:paragraph (:begin 423 :end 694 :contents-begin 423 :contents-end 693)
      "Radio-link boundary: <<<unmatched radio marker.
Plain-link boundary: https: spaced-path.
Regular-link boundary: [[bracket-path][label].
Macro boundary: {{{unterminated macro.
Target boundary: <<unterminated target.
Radio-target boundary: <"
      (:target (:begin 662 :end 691 :contents-begin nil :contents-end nil :value "unterminated radio target"))
      ".
"
    )
    (:paragraph (:begin 694 :end 868 :contents-begin 694 :contents-end 868)
      "Statistics boundaries: [/ ] "
      (:statistics-cookie (:begin 722 :end 729 :contents-begin nil :contents-end nil :value "[101%]" :type nil))
      "[2 / 5].
Subscript boundary: word_ and spaced x_ y.
Superscript boundary: x^ and x^{unclosed.
Table row edge with omitted final separator:
"
    )
    (:table (:begin 868 :end 906 :contents-begin 868 :contents-end 906 :type :org :tblfm nil)
      (:table-row (:begin 868 :end 906 :contents-begin 869 :contents-end 905 :type :standard)
        (:table-cell (:begin 869 :end 905 :contents-begin 870 :contents-end 905 :value nil)
          "final cell is not followed by a bar"
        )
      )
    )
    (:paragraph (:begin 906 :end 943 :contents-begin 906 :contents-end 942)
      "Timestamp boundary: <2026-10-12 Mon
"
    )
    (:paragraph (:begin 943 :end 1128 :contents-begin 943 :contents-end 1128)
      "Markup boundaries: *unclosed bold, /unclosed italic, _unclosed underline,
+unclosed strike, =unclosed verbatim, and ~unclosed code.
Angle-link boundary: <"
      (:link (:begin 1097 :end 1126 :contents-begin nil :contents-end nil :type "https" :path "unterminated-angle-path" :format :plain))
      ".
"
    )
  )
)
