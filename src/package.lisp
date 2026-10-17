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
   #:org-table-unsupported-syntax
   #:org-table-unsupported-syntax-formula
   #:org-table-unsupported-syntax-position
   #:org-table-unsupported-syntax-feature
   #:org-table-error #:org-table-error-target #:org-table-error-reason
   #:org-resource-limit-error #:org-resource-limit-error-limit-name
   #:org-resource-limit-error-limit #:org-resource-limit-error-actual
   #:org-callback-error #:org-callback-error-kind #:org-callback-error-name
   #:org-callback-error-reason #:org-elisp-evaluator-error #:org-callback-timeout-error
   #:org-write-plan-error #:org-write-plan-error-path #:org-write-plan-error-reason
   #:org-write-plan-error-cause
   #:org-id-error #:org-id-error-reason
   #:org-link-resolution-error #:org-link-resolution-error-reason
   #:org-link-resolution-error-context
   #:org-store-link-error #:org-store-link-error-reason
   #:org-store-link-error-context
   ;; Clock aggregation
   #:org-clock-error #:org-clock-error-reason #:org-clock-error-context
   #:clock-summary #:make-clock-summary #:clock-summary-p
   #:clock-summary-scoped-minutes #:clock-summary-unscoped-minutes
   #:clock-summary-total-minutes #:clock-summary-clipped-interval-count
   #:sum-clocks
   #:clocktable-row #:make-clocktable-row #:clocktable-row-p
   #:clocktable-row-level #:clocktable-row-title #:clocktable-row-tags
   #:clocktable-row-range #:clocktable-row-minutes
   #:clocktable-result #:make-clocktable-result #:clocktable-result-p
   #:clocktable-result-total-minutes #:clocktable-result-rows
   #:clocktable-result-range #:clocktable-data
   #:clock-consistency-issue #:make-clock-consistency-issue
   #:clock-consistency-issue-p #:clock-consistency-issue-reason
   #:clock-consistency-issue-line #:clock-consistency-issue-other-line
   #:clock-consistency-issue-duration-minutes #:check-clock-consistency
   #:timer-item-value #:insert-timer-item
   ;; Clock editing
   #:clock-in #:clock-out
   #:write-clocktable
   ;; Edit results
   #:edit-span #:make-edit-span #:edit-span-p #:edit-span-start #:edit-span-end
   #:edit-result #:make-edit-result #:edit-result-p
   #:edit-result-updated-document #:edit-result-spans
   ;; Resource limits
   #:limits #:make-limits #:limits-p #:limit-value
   ;; Explicit callback registry
   #:callback-registry #:make-callback-registry #:callback-registry-p
   #:register-callback #:invoke-callback #:evaluate-elisp
   ;; Write plans
   #:write-plan-entry #:make-write-plan-entry #:write-plan-entry-p
   #:write-plan-entry-path #:write-plan-entry-content #:write-plan-entry-mode
   #:write-plan-entry-create-directories-p
   #:write-plan #:make-write-plan #:write-plan-p #:write-plan-entries
   #:apply-write-plan
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
   #:parse-org #:write-org #:emit-org
   ;; Format-preserving headline edits
   #:set-heading-todo #:set-heading-priority #:adjust-heading-priority
   #:replace-heading-title
   #:add-heading-tags #:remove-heading-tags #:replace-heading-tags
   ;; TODO workflow transitions
   #:org-workflow-options #:make-org-workflow-options #:org-workflow-options-p
   #:org-workflow-options-log-done #:org-workflow-options-log-repeat
   #:org-workflow-options-repeat-to-state
   #:org-workflow-options-log-into-drawer
   #:org-workflow-options-clock-into-drawer
   #:transition-result #:make-transition-result #:transition-result-p
   #:transition-result-previous-state #:transition-result-new-state
   #:transition-result-log-events #:transition-result-rescheduled-timestamps
   #:transition-result-closed-timestamp
   #:cycle-heading-todo #:transition-heading-todo
   ;; Format-preserving metadata edits
   #:set-property #:remove-property #:set-planning #:remove-planning
   #:set-keyword-value #:remove-keyword
   ;; Format-preserving element edits
   #:replace-block-body #:set-checkbox-state
   ;; Format-preserving plain list edits
   #:insert-list-item #:delete-list-item #:move-list-item
   #:indent-list-item #:unindent-list-item #:set-list-bullet
   #:renumber-ordered-list
   ;; Format-preserving footnote edits
   #:insert-footnote #:renumber-footnotes #:normalize-footnotes
   ;; Format-preserving drawer edits
   #:set-drawer-body #:remove-drawer
   ;; Format-preserving table edits
   #:table-align #:table-insert-row #:table-delete-row #:table-move-row
   #:table-insert-column #:table-delete-column #:table-move-column
   #:table-sort-rows
   ;; Statistics cookies
   #:statistics-cookie-update #:make-statistics-cookie-update
   #:statistics-cookie-update-p #:statistics-cookie-update-node
   #:statistics-cookie-update-old-value #:statistics-cookie-update-new-value
   #:update-statistics-cookies
   #:table-recalculate
   ;; Format-preserving subtree edits
   #:insert-subtree #:delete-subtree #:replace-subtree
   #:promote-subtree #:demote-subtree
   ;; Unit E subtree relocation and sort
   #:relocation-result #:make-relocation-result #:relocation-result-p
   #:relocation-result-source-edit #:relocation-result-target-edit
   #:relocation-result-mode #:relocation-result-source-path
   #:relocation-result-target-path
   #:move-subtree #:refile-subtree #:set-heading-archive-tag
   #:archive-subtree-to #:sort-subtree-children
   ;; Queries
   #:query-context #:make-query-context #:query-context-p
   #:query-context-tag-groups #:query-context-property-inheritance-p
   #:query-context-tag-inheritance-p #:query-context-file #:query-context-category
   #:query-context-blocked-provider #:query-context-clock-sum-provider
   #:query-context-clock-sum-today-provider #:query-context-case-fold-p
   #:query-list #:query-path #:query-match-at #:query-match-now
   #:node-heading-path
   ;; Timestamp values
   #:timestamp-start #:timestamp-end #:timestamp-repeater #:timestamp-warning-delay
   ;; Unit V timestamp facade and time context
   #:org-time-error #:org-time-error-reason #:org-time-error-context
   #:org-time-value #:make-org-time-value #:org-time-value-p
   #:org-time-value-kind #:org-time-value-iso8601 #:org-time-value-year
   #:org-time-value-month #:org-time-value-day #:org-time-value-hour
   #:org-time-value-minute #:org-time-value-second #:org-time-value-offset-minutes
   #:org-time-zone #:make-fixed-time-zone #:resolve-time-zone #:org-time-zone-p
   #:org-time-zone-name #:org-time-zone-offset-minutes
   #:org-time-context #:make-time-context #:org-time-context-p
   #:org-time-context-reference-time #:org-time-context-zone
   #:org-time-context-dst-policy #:org-time-context-reference-instant
   #:org-time-range #:make-time-range #:org-time-range-p
   #:org-time-range-start #:org-time-range-end
   #:org-time-range-include-start-p #:org-time-range-include-end-p
   #:org-timestamp-view #:make-org-timestamp-view #:org-timestamp-view-p
   #:org-timestamp-view-kind #:org-timestamp-view-start #:org-timestamp-view-end
   #:org-timestamp-view-start-inclusive-p #:org-timestamp-view-end-inclusive-p
   #:org-timestamp-view-repeater-amount #:org-timestamp-view-repeater-unit
   #:org-timestamp-view-repeater-mode #:org-timestamp-view-warning-amount
   #:org-timestamp-view-warning-unit #:org-timestamp-view-warning-mode
   #:timestamp-view
   #:org-time-interval-kind #:org-time-interval-amount #:org-time-interval-unit
   ;; F agenda
   #:org-agenda-error #:org-agenda-error-reason #:org-agenda-error-context
   #:agenda-entry #:make-agenda-entry #:agenda-entry-p
   #:agenda-entry-date #:agenda-entry-kind #:agenda-entry-headline
   #:agenda-entry-heading-path #:agenda-entry-timestamp
   #:agenda-entry-warning-state #:agenda-entry-scheduled-delay-state
   #:agenda-entry-todo-state #:agenda-entry-effective-tags
   #:agenda-entry-habit-p #:agenda-entry-source-node
   #:agenda-result #:make-agenda-result #:agenda-result-p
   #:agenda-result-entries #:agenda-result-start-date #:agenda-result-end-date
   #:agenda-result-time-context
   #:agenda-match-entry #:make-agenda-match-entry #:agenda-match-entry-p
   #:agenda-match-entry-document #:agenda-match-entry-node
   #:agenda-match-entry-heading-path #:agenda-match-entry-line
   #:agenda-match-entry-end-line
   #:agenda #:agenda-todo-list #:agenda-match
   ;; Effective Org semantics
   #:effective-heading-tags #:effective-property #:startup-options
   ;; Org IDs
   #:org-id-index #:make-org-id-index #:org-id-index-p
   #:org-id-index-candidates #:generate-org-id #:set-heading-id
   ;; Link resolution and storage
   #:link-resolution #:make-link-resolution #:link-resolution-p
   #:link-resolution-status #:link-resolution-target
   #:link-resolution-candidates #:resolve-link #:store-link
   ;; Unit E1 export context and conditions
   #:export-context #:make-export-context #:export-context-p
   #:export-context-title #:export-context-author #:export-context-date
   #:export-context-language #:export-context-time-context
   #:org-export-error #:org-export-error-backend #:org-export-error-reason
   #:org-export-error-source
   #:org-export-unsupported-syntax
   #:org-export-unsupported-syntax-backend
   #:org-export-unsupported-syntax-source
   #:org-export-unsupported-syntax-position
   #:org-export-unsupported-syntax-feature
   ;; Unit E2 HTML export
   #:export-org))
