;;;; src/package.lisp
(defpackage #:org-kit
  (:use #:cl)
  (:export
   ;; Node types
   #:node-type #:node-types #:node-type-p #:node-type-category
   ;; Conditions
   #:org-kit-error #:org-kit-error-message
   #:org-parse-error #:org-parse-error-offset #:org-parse-error-line
   #:org-parse-error-column #:org-parse-error-context #:org-parse-error-source-name
   #:org-query-error #:org-query-error-query #:org-query-error-position
   #:org-edit-error #:org-edit-error-target #:org-edit-error-reason
   #:org-resource-limit-error #:org-resource-limit-error-limit-name
   #:org-resource-limit-error-limit #:org-resource-limit-error-actual
   ;; Resource limits
   #:limits #:make-limits #:limits-p #:limit-value
   ;; Documents
   #:document #:make-document #:document-p #:document-text #:document-encoding
   #:document-bom-p #:document-source-name #:document-limits #:document-settings #:document-root
   #:document-line-count #:document-newline #:document-final-newline-p
   #:offset-line-column #:line-offset
   ;; Nodes
   #:node #:make-node #:node-p #:node-parent #:node-children
   #:append-child #:node-begin #:node-end #:node-contents-begin #:node-contents-end
   #:node-post-affiliated #:node-post-blank #:node-property #:node-properties
   #:node-root #:node-document #:node-text #:node-start-position
   #:node-end-position #:node-line-range #:map-nodes
   ;; Outline
   #:outline #:outline-entry #:make-outline-entry #:outline-entry-p
   #:outline-entry-kind #:outline-entry-name #:outline-entry-level
   #:outline-entry-line #:outline-entry-end-line #:outline-entry-node
   #:outline-result #:make-outline-result #:outline-result-p
   #:outline-result-path #:outline-result-lang #:outline-result-symbols
   #:outline-result-total #:outline-result-truncated
   ;; Reading and writing
   #:parse-org #:write-org
   ;; Format-preserving headline edits
   #:set-heading-todo #:set-heading-priority #:replace-heading-title
   #:add-heading-tags #:remove-heading-tags #:replace-heading-tags
   ;; Format-preserving metadata edits
   #:set-property #:remove-property #:set-planning #:remove-planning
   #:set-keyword-value #:remove-keyword
   ;; Timestamp values
   #:timestamp-start #:timestamp-end #:timestamp-repeater #:timestamp-warning-delay))
