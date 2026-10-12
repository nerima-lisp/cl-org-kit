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
(:org-data (:begin 0 :end 58 :contents-begin 0 :contents-end 58)
  (:section (:begin 0 :end 23 :contents-begin 0 :contents-end 23)
    (:keyword (:begin 0 :end 23 :contents-begin nil :contents-end nil :key "TITLE" :value "CRLF fixture" :options nil))
  )
  (:headline (:begin 23 :end 58 :contents-begin 39 :contents-end 58 :title ("CRLF heading") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil)
    (:section (:begin 39 :end 58 :contents-begin 39 :contents-end 58)
      (:paragraph (:begin 39 :end 58 :contents-begin 39 :contents-end 58)
        "日本語の行に絵文字 🐈 を含める。
"
      )
    )
  )
)
