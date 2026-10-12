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
(:org-data (:begin 0 :end 310 :contents-begin 0 :contents-end 310)
  (:section (:begin 0 :end 37 :contents-begin 0 :contents-end 37)
    (:keyword (:begin 0 :end 37 :contents-begin nil :contents-end nil :key "TITLE" :value "Inline task level boundary" :options nil))
  )
  (:headline (:begin 37 :end 267 :contents-begin 98 :contents-end 267 :title ("Fourteen stars are below the default minimum.") :level 14 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
    (:section (:begin 98 :end 267 :contents-begin 98 :contents-end 267)
      (:inlinetask (:begin 98 :end 210 :contents-begin 159 :contents-end 190 :title ("Fifteen stars meet the default minimum.") :level 15 :todo-keyword "TODO" :todo-type :todo :priority nil :tags nil :commentedp nil :archivedp nil)
        (:paragraph (:begin 159 :end 190 :contents-begin 159 :contents-end 190)
          "An inline-task body paragraph.
"
        )
      )
      (:inlinetask (:begin 210 :end 267 :contents-begin nil :contents-end nil :title ("Inline task without an explicit END") :level 15 :todo-keyword "TODO" :todo-type :todo :priority nil :tags nil :commentedp nil :archivedp nil))
    )
  )
  (:headline (:begin 267 :end 310 :contents-begin nil :contents-end nil :title ("A regular heading after the inline tasks") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil))
)
