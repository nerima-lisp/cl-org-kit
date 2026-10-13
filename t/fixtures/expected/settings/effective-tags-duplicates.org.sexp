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
; :effective-tags is captured from org-get-tags at each headline.
(:org-data (:begin 0 :end 132 :contents-begin 0 :contents-end 132)
  (:section (:begin 0 :end 44 :contents-begin 0 :contents-end 44)
    (:keyword (:begin 0 :end 44 :contents-begin nil :contents-end nil :key "FILETAGS" :value ":file_before:shared:file_after:" :options nil))
  )
  (:headline (:begin 44 :end 132 :contents-begin 89 :contents-end 132 :title ("Parent") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags ("parent_before" "shared" "parent_after") :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil :effective-tags ("file_before" "file_after" "parent_before" "shared" "parent_after"))
    (:headline (:begin 89 :end 132 :contents-begin nil :contents-end nil :title ("Child") :level 2 :todo-keyword nil :todo-type nil :priority nil :tags ("child_before" "shared" "child_after") :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil :effective-tags ("file_before" "file_after" "parent_before" "parent_after" "child_before" "shared" "child_after")))
  )
)
