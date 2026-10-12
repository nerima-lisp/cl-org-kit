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
(:org-data (:begin 0 :end 702 :contents-begin 0 :contents-end 702)
  (:section (:begin 0 :end 702 :contents-begin 0 :contents-end 702)
    (:keyword (:begin 0 :end 23 :contents-begin nil :contents-end nil :key "TITLE" :value "Text objects" :options nil))
    (:paragraph (:begin 23 :end 100 :contents-begin 23 :contents-end 99)
      "Plain text remains a plain-text object unless a more specific form matches.
"
    )
    (:paragraph (:begin 100 :end 237 :contents-begin 100 :contents-end 236)
      "Entity forms: "
      (:entity (:begin 114 :end 120 :contents-begin nil :contents-end nil :name "alpha" :latex "\\alpha" :latex-math-p t :html "&alpha;" :ascii "alpha" :utf-8 "α" :use-brackets-p nil))
      ", "
      (:entity (:begin 122 :end 127 :contents-begin nil :contents-end nil :name "nbsp" :latex "~" :latex-math-p nil :html "&nbsp;" :ascii " " :utf-8 " " :use-brackets-p nil))
      ", and a whitespace entity "
      (:entity (:begin 153 :end 158 :contents-begin nil :contents-end nil :name "_   " :latex "\\hspace*{1.5em}" :latex-math-p nil :html "&ensp;&ensp;&ensp;" :ascii "   " :utf-8 "   " :use-brackets-p nil))
      ".
Boundary entity spelling: "
      (:latex-fragment (:begin 186 :end 199 :contents-begin nil :contents-end nil :value "\\notAnEntity"))
      "and incomplete "
      (:entity (:begin 214 :end 220 :contents-begin nil :contents-end nil :name "alpha" :latex "\\alpha" :latex-math-p t :html "&alpha;" :ascii "alpha" :utf-8 "α" :use-brackets-p nil))
      "{ stay as text.
"
    )
    (:paragraph (:begin 237 :end 347 :contents-begin 237 :contents-end 346)
      "LaTeX fragments: "
      (:latex-fragment (:begin 254 :end 263 :contents-begin nil :contents-end nil :value "\\(a + b\\)"))
      ", "
      (:latex-fragment (:begin 265 :end 284 :contents-begin nil :contents-end nil :value "\\[c^2 = a^2 + b^2\\]"))
      ", and "
      (:entity (:begin 290 :end 296 :contents-begin nil :contents-end nil :name "alpha" :latex "\\alpha" :latex-math-p t :html "&alpha;" :ascii "alpha" :utf-8 "α" :use-brackets-p nil))
      (:subscript (:begin 296 :end 300 :contents-begin 298 :contents-end 299 :use-brackets-p t)
        "i"
      )
      ".
Boundary fragment: an unclosed \\( fragment.
"
    )
    (:paragraph (:begin 347 :end 592 :contents-begin 347 :contents-end 591)
      "Six markup forms: "
      (:bold (:begin 365 :end 371 :contents-begin 366 :contents-end 370)
        "bold"
      )
      ", "
      (:italic (:begin 373 :end 381 :contents-begin 374 :contents-end 380)
        "italic"
      )
      ", "
      (:underline (:begin 383 :end 394 :contents-begin 384 :contents-end 393)
        "underline"
      )
      ", "
      (:strike-through (:begin 396 :end 412 :contents-begin 397 :contents-end 411)
        "strike-through"
      )
      ",
"
      (:verbatim (:begin 414 :end 429 :contents-begin nil :contents-end nil))
      ", and "
      (:code (:begin 435 :end 446 :contents-begin nil :contents-end nil))
      ".
Markup boundaries: a * spaced * marker, / spaced / marker, and x _ y.
Code and verbatim contents stay literal: "
      (:code (:begin 559 :end 572 :contents-begin nil :contents-end nil))
      "and "
      (:verbatim (:begin 576 :end 590 :contents-begin nil :contents-end nil))
      "
"
    )
    (:paragraph (:begin 592 :end 702 :contents-begin 592 :contents-end 702)
      "Subscript and superscript: H"
      (:subscript (:begin 620 :end 623 :contents-begin 621 :contents-end 623 :use-brackets-p nil)
        "2O"
      )
      ", x"
      (:superscript (:begin 626 :end 628 :contents-begin 627 :contents-end 628 :use-brackets-p nil)
        "3"
      )
      ", x"
      (:subscript (:begin 631 :end 637 :contents-begin 633 :contents-end 636 :use-brackets-p t)
        "i+1"
      )
      ", and x"
      (:superscript (:begin 644 :end 652 :contents-begin 646 :contents-end 651 :use-brackets-p t)
        "y"
        (:superscript (:begin 647 :end 651 :contents-begin 649 :contents-end 650 :use-brackets-p t)
          "z"
        )
      )
      ".
Subscript boundary: snake"
      (:subscript (:begin 679 :end 685 :contents-begin 680 :contents-end 684 :use-brackets-p nil)
        "case"
      )
      "and spaced x_ y.
"
    )
  )
)
