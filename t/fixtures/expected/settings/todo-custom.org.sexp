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
(:org-data (:begin 0 :end 428 :contents-begin 0 :contents-end 428)
  (:section (:begin 0 :end 168 :contents-begin 0 :contents-end 168)
    (:keyword (:begin 0 :end 39 :contents-begin nil :contents-end nil :key "TITLE" :value "Custom TODO keyword sequences" :options nil))
    (:keyword (:begin 39 :end 91 :contents-begin nil :contents-end nil :key "TODO" :value "TODO(t) WAIT(w@/!) | DONE(d!) CANCELLED(c@)" :options nil))
    (:keyword (:begin 91 :end 133 :contents-begin nil :contents-end nil :key "SEQ_TODO" :value "NEXT(n) HOLD(h) | FINISHED(f)" :options nil))
    (:keyword (:begin 133 :end 168 :contents-begin nil :contents-end nil :key "TYP_TODO" :value "IDEA(i) | REJECTED(r)" :options nil))
  )
  (:headline (:begin 168 :end 195 :contents-begin nil :contents-end nil :title ("Basic sequence todo") :level 1 :todo-keyword "TODO" :todo-type :todo :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil))
  (:headline (:begin 195 :end 225 :contents-begin nil :contents-end nil :title ("Active state from TODO") :level 1 :todo-keyword "WAIT" :todo-type :todo :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil))
  (:headline (:begin 225 :end 258 :contents-begin nil :contents-end nil :title ("Completed state from TODO") :level 1 :todo-keyword "DONE" :todo-type :done :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil))
  (:headline (:begin 258 :end 291 :contents-begin nil :contents-end nil :title ("Sequential workflow state") :level 1 :todo-keyword "NEXT" :todo-type :todo :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil))
  (:headline (:begin 291 :end 329 :contents-begin nil :contents-end nil :title ("Completed sequential state") :level 1 :todo-keyword "FINISHED" :todo-type :done :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil))
  (:headline (:begin 329 :end 357 :contents-begin nil :contents-end nil :title ("Typed workflow state") :level 1 :todo-keyword "IDEA" :todo-type :todo :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil))
  (:headline (:begin 357 :end 390 :contents-begin nil :contents-end nil :title ("Completed typed state") :level 1 :todo-keyword "REJECTED" :todo-type :done :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil))
  (:headline (:begin 390 :end 428 :contents-begin nil :contents-end nil :title ("UNKNOWN Remains ordinary title text") :level 1 :todo-keyword nil :todo-type nil :priority nil :tags nil :commentedp nil :archivedp nil :scheduled nil :deadline nil :closed nil))
)
