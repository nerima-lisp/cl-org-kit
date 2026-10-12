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
(:org-data (:begin 0 :end 591 :contents-begin 0 :contents-end 591)
  (:section (:begin 0 :end 179 :contents-begin 0 :contents-end 179)
    (:keyword (:begin 0 :end 56 :contents-begin nil :contents-end nil :key "TITLE" :value "Org structure: headings, sections, 日本語, and 🧭" :options nil))
    (:paragraph (:begin 56 :end 179 :contents-begin 56 :contents-end 178)
      "This is the zeroth section. It contains plain text before the first heading.
The second line stays in the same paragraph.
"
    )
  )
  (:headline (:begin 179 :end 378 :contents-begin 204 :contents-end 378 :title ("Heading with a section") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
    (:section (:begin 204 :end 239 :contents-begin 204 :contents-end 239)
      (:paragraph (:begin 204 :end 239 :contents-begin 204 :contents-end 239)
        "This section contains a paragraph.
"
      )
    )
    (:headline (:begin 239 :end 378 :contents-begin 305 :contents-end 378 :title ("Annotated heading with 日本語 and 🧪") :level 2 :todo-keyword "TODO" :todo-type :todo :priority 65 :tags ("work" "docs") :commentedp t :archivedp nil :scheduled nil :deadline nil :closed nil)
      (:section (:begin 305 :end 336 :contents-begin 305 :contents-end 336)
        (:paragraph (:begin 305 :end 336 :contents-begin 305 :contents-end 336)
          "This child heading has a body.
"
        )
      )
      (:headline (:begin 336 :end 378 :contents-begin 351 :contents-end 378 :title ("Deep child") :level 3 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
        (:section (:begin 351 :end 378 :contents-begin 351 :contents-end 378)
          (:paragraph (:begin 351 :end 378 :contents-begin 351 :contents-end 378)
            "Text under the deep child.
"
          )
        )
      )
    )
  )
  (:headline (:begin 378 :end 467 :contents-begin 401 :contents-end 467 :title ("Empty section parent") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
    (:headline (:begin 401 :end 467 :contents-begin 429 :contents-end 467 :title ("Child begins immediately") :level 2 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
      (:section (:begin 429 :end 467 :contents-begin 429 :contents-end 467)
        (:paragraph (:begin 429 :end 467 :contents-begin 429 :contents-end 467)
          "The parent has no section of its own.
"
        )
      )
    )
  )
  (:headline (:begin 467 :end 591 :contents-begin 504 :contents-end 591 :title ("Archived heading") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags ("ARCHIVE" "history") :commentedp nil :archivedp t :scheduled nil :deadline nil :closed nil)
    (:section (:begin 504 :end 591 :contents-begin 504 :contents-end 591)
      (:paragraph (:begin 504 :end 591 :contents-begin 504 :contents-end 591)
        "Archived content.
*No space after the star is not a heading
This is another paragraph.
"
      )
    )
  )
)
