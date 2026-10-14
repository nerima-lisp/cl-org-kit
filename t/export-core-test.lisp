;;;; t/export-core-test.lisp
(in-package #:cl-org-kit/test)

(defun export-core-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun export-core-test-views (views)
  (mapcan (lambda (view)
            (cons view (export-core-test-views
                        (org-kit::export-view-children view))))
          views))

(defun export-core-test-all-views (preparation)
  (export-core-test-views
   (org-kit::export-preparation-root-views preparation)))

(defun export-core-test-view-nodes (preparation type)
  (remove-if-not (lambda (view)
                   (eq (node-type (org-kit::export-view-node view)) type))
                 (export-core-test-all-views preparation)))

(defun export-core-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(defun export-core-test-preparation (source &rest arguments)
  (apply #'org-kit::export-prepare (parse-org source) :html arguments))

(defun export-core-test-policy-fixture-source (name)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "cl-org-kit"
    (concatenate 'string "t/fixtures/export/policy/core/" name))))

(defun export-core-test-preparation-with-options (options)
  (export-core-test-preparation
   (format nil "#+OPTIONS: ~A~%~A" options
           (export-core-test-policy-fixture-source "options-pruning.org"))))

(defun export-core-test-limits (name value)
  (apply #'make-limits (list name value)))

(defun export-core-test-prepare-with-limit (source name value)
  (org-kit::export-prepare
   (parse-org source :limits (export-core-test-limits name value)) :html))

(defun export-core-test-document-snapshot (document)
  (let ((nodes nil))
    (map-nodes
     (lambda (node)
       (push (list node (node-parent node) (copy-list (node-children node))
                   (node-begin node) (node-end node)
                   (node-contents-begin node) (node-contents-end node)
                   (node-post-blank node) (copy-tree (node-properties node)))
             nodes))
     (document-root document))
    (list (document-text document) (document-source-name document)
          (copy-tree (document-settings document)) (nreverse nodes))))

(defun export-core-test-source-nodes (preparation type)
  (mapcar #'org-kit::export-view-node
          (export-core-test-view-nodes preparation type)))

(defclass export-core-test-binary-output-stream
    (sb-gray:fundamental-binary-output-stream)
  ((element-type :initarg :element-type :reader export-core-test-element-type)
   (octets :initform (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0)
           :reader export-core-test-stream-octets)))

(defmethod stream-element-type ((stream export-core-test-binary-output-stream))
  (export-core-test-element-type stream))

(defmethod sb-gray:stream-write-byte
    ((stream export-core-test-binary-output-stream) byte)
  (vector-push-extend byte (export-core-test-stream-octets stream)))

(describe "E1 export context and core preparation"
  (it "runs the export policy fixtures through preparation"
    (let ((cases '("footnotes-after-pruning.org"
                   "include-callback.org"
                   "literal-macro.org"
                   "options-pruning.org"
                   "outputless-keywords.org"
                   "standard-options.org"
                   "tex-pruning.org"
                   "selection-pruning.org"
                   "selection-source-blocks.org"
                   "source-export-modes.org"
                   "recursive-macro.org"
                   "unsupported-diary.org")))
      (with-soft-assertions
        (dolist (name cases)
          (let* ((source (export-core-test-policy-fixture-source name))
                 (condition
                   (export-core-test-catch-error
                    (lambda ()
                      (org-kit::export-prepare
                       (parse-org source) :html
                       :include-resolver (lambda (path parent-source)
                                           (declare (ignore path parent-source))
                                           "Included paragraph"))))))
            (if (member name '("recursive-macro.org" "unsupported-diary.org")
                        :test #'string=)
                (progn
                  (expect (typep condition 'org-export-unsupported-syntax)
                          :to-be t)
                  (when (typep condition 'org-export-unsupported-syntax)
                    (expect (org-export-unsupported-syntax-feature condition)
                            :to-be (if (string= name "recursive-macro.org")
                                       :recursive-macro :diary-sexp))))
                (expect condition :to-be nil)))))))

  (it "keeps caller metadata immutable and applies caller overrides"
    (let* ((context (make-export-context :title "Caller title"
                                         :author "Caller"
                                         :date "2030-01-02"
                                         :language "fr"))
           (document
             (parse-org (export-core-test-lines
                         "#+TITLE: Document title"
                         "#+AUTHOR: Document author"
                         "#+DATE: 2026-10-13"
                         "#+LANGUAGE: ja"
                         "Body")))
           (preparation (org-kit::export-prepare document :html :context context))
           (metadata (org-kit::export-preparation-metadata preparation)))
      (with-soft-assertions
        (expect (export-context-p context) :to-be t)
        (expect (export-context-title context) :to-equal "Caller title")
        (expect (export-context-author context) :to-equal "Caller")
        (expect (export-context-date context) :to-equal "2030-01-02")
        (expect (export-context-language context) :to-equal "fr")
        (expect (export-context-time-context context) :to-be nil)
        (expect (getf metadata :title) :to-equal "Caller title")
        (expect (getf metadata :author) :to-equal "Caller")
        (expect (getf metadata :date) :to-equal "2030-01-02")
        (expect (getf metadata :language) :to-equal "fr")
        (expect (getf metadata :time-context) :to-be nil)
        (expect (export-context-p nil) :to-be nil)
        (expect (typep (export-core-test-catch-error
                        (lambda () (make-export-context :title 42)))
                       'type-error)
                :to-be t))))

  (it "uses stable metadata defaults without consulting ambient state"
    (let* ((preparation (export-core-test-preparation "Body"))
           (metadata (org-kit::export-preparation-metadata preparation)))
      (with-soft-assertions
        (expect (getf metadata :title) :to-be nil)
        (expect (getf metadata :author) :to-be nil)
        (expect (getf metadata :date) :to-be nil)
        (expect (getf metadata :language) :to-equal "en")
        (expect (getf metadata :time-context) :to-be nil))))

  (it "keeps an explicit empty metadata override distinct from NIL"
    (let* ((context (make-export-context :title ""))
           (preparation
             (org-kit::export-prepare (parse-org "#+TITLE: Document title")
                                      :html :context context)))
      (with-soft-assertions
        (expect (export-context-title context) :to-equal "")
        (expect (getf (org-kit::export-preparation-metadata preparation) :title)
                :to-equal ""))))

  (it "accepts outputless keywords and ignores keywords in pruned subtrees"
    (let* ((source (export-core-test-policy-fixture-source
                    "outputless-keywords.org"))
           (preparation (export-core-test-preparation source))
           (paragraphs (export-core-test-view-nodes preparation :paragraph))
           (paragraph-texts
             (mapcar (lambda (view)
                       (node-text (org-kit::export-view-node view)))
                     paragraphs)))
      (with-soft-assertions
        (expect paragraphs :not :to-be nil)
        (expect (some (lambda (text) (search "Visible text." text))
                      paragraph-texts)
                :to-be-truthy))))

  (it "parses standard options across lines and applies TODO and archive pruning"
    (let* ((source (export-core-test-policy-fixture-source "standard-options.org"))
           (preparation (export-core-test-preparation source))
           (options (org-kit::export-preparation-options preparation))
           (headlines (export-core-test-view-nodes preparation :headline))
           (paragraphs (export-core-test-view-nodes preparation :paragraph))
           (paragraph-texts
             (mapcar (lambda (view)
                       (node-text (org-kit::export-view-node view)))
                     paragraphs))
           (task-headings-for-mode
             (lambda (mode)
               (let* ((task-source
                        (format nil
                                "#+OPTIONS: tasks:~A~%#+TODO: TODO | DONE~%* TODO Active~%* DONE Completed~%* Normal"
                                mode))
                      (task-preparation
                        (export-core-test-preparation task-source)))
                 (mapcar (lambda (view)
                           (let ((node (org-kit::export-view-node view)))
                             (list (node-property node :raw-value)
                                   (node-property node :todo-keyword)
                                   (node-property node :todo-type))))
                         (export-core-test-view-nodes task-preparation :headline)))))
           (tasks-nil (funcall task-headings-for-mode "nil"))
           (tasks-todo (funcall task-headings-for-mode "todo"))
           (tasks-done (funcall task-headings-for-mode "done")))
      (with-soft-assertions
        (expect (length options) :to-be 33)
        (expect (cdr (assoc :toc options)) :to-be 7)
        (expect (cdr (assoc :heading-depth options)) :to-be 7)
        (expect (cdr (assoc :preserve-breaks options)) :to-be t)
        (expect (mapcar (lambda (view)
                          (let ((node (org-kit::export-view-node view)))
                            (list (node-property node :raw-value)
                                  (node-property node :todo-keyword)
                                  (node-property node :todo-type))))
                        headlines)
                :to-equal '(("Active" "TODO" :todo)
                            ("Normal" nil nil)
                            ("Archived" nil nil)))
        (expect (some (lambda (text) (search "Archived body." text))
                      paragraph-texts)
                :to-be nil)
        (expect tasks-nil :to-equal '(("Normal" nil nil)))
        (expect tasks-todo
                :to-equal '(("Active" "TODO" :todo) ("Normal" nil nil)))
        (expect tasks-done
                :to-equal '(("Completed" "DONE" :done) ("Normal" nil nil))))))

  (it "prunes parsed LaTeX nodes when tex is nil"
    (let* ((preparation
             (export-core-test-preparation
              (export-core-test-policy-fixture-source "tex-pruning.org")))
           (options (org-kit::export-preparation-options preparation)))
      (with-soft-assertions
        (expect (cdr (assoc :latex options)) :to-be nil)
        (expect (export-core-test-view-nodes preparation :latex-fragment)
                :to-be nil)
        (expect (export-core-test-view-nodes preparation :latex-environment)
                :to-be nil))))

  (it "parses standard option values and preserves fixed-width content in the view"
    (let* ((cases
             '(("toc:nil" :toc nil)
               ("toc:t" :toc t)
               ("toc:2 toc:7" :toc 7)
               ("h:0" :heading-depth 0)
               ("num:nil" :num nil)
               ("num:t" :num t)
               ("^:t" :^ t)
               ("^:nil" :^ nil)
               ("^:{}" :^ "{}")
               ("::t" :fixed-width t)
               ("::nil" :fixed-width nil)
               ("arch:nil" :arch nil)
               ("arch:t" :arch t)
               ("arch:headline" :arch :headline)
               ("tex:nil" :latex nil)
               ("tex:t" :latex t)
               ("tex:verbatim" :latex :verbatim)
               ("tags:nil" :tags nil)
               ("tags:t" :tags t)
               ("tags:not-in-toc" :tags :not-in-toc)
               ("<:nil" :timestamp-range nil)
               ("<:t" :timestamp-range t)
               ("<:active" :timestamp-range :active)
               ("<:active-exclude-diary" :timestamp-range :active-exclude-diary)
               ("<:inactive" :timestamp-range :inactive)
               ("c:nil" :clock nil)
               ("c:t" :clock t)
               ("creator:nil" :creator nil)
               ("d:nil" :drawers nil)
               ("d:t" :drawers t)
               ("prop:nil" :properties nil)
               ("prop:t" :properties t)
               ("tasks:nil" :tasks nil)
               ("tasks:t" :tasks t)
               ("tasks:todo" :tasks :todo)
               ("tasks:done" :tasks :done)
               ("todo:nil" :todo nil)
               ("todo:t" :todo t)))
           (actual
             (mapcar
              (lambda (case)
                (let* ((preparation
                         (export-core-test-preparation-with-options (first case)))
                       (option (assoc (second case)
                                      (org-kit::export-preparation-options
                                       preparation))))
                  (list (first case) (cdr option))))
              cases))
           (expected
             (mapcar (lambda (case) (list (first case) (third case))) cases))
           (fixed-width-disabled
             (export-core-test-preparation-with-options "::nil")))
      (with-soft-assertions
        (expect actual :to-equal expected)
        (expect (export-core-test-view-nodes fixed-width-disabled :fixed-width)
                :to-be-truthy))))

  (it "applies included and excluded option lists to parsed drawers and properties"
    (let* ((included
             (export-core-test-preparation-with-options
              "d:(\"LOGBOOK\") prop:(\"CUSTOM_ID\" \"UNUSED ITEM\")"))
           (included-options (org-kit::export-preparation-options included))
           (included-drawers
             (mapcar (lambda (view)
                       (node-property (org-kit::export-view-node view) :drawer-name))
                     (export-core-test-view-nodes included :drawer)))
           (included-property-drawers
             (export-core-test-view-nodes included :property-drawer))
           (excluded
             (export-core-test-preparation-with-options
              "d:(not LOGBOOK)"))
           (excluded-drawers
             (mapcar (lambda (view)
                       (node-property (org-kit::export-view-node view) :drawer-name))
                     (export-core-test-view-nodes excluded :drawer)))
           (no-matching-property
             (export-core-test-preparation-with-options "prop:(UNUSED ITEM)")))
      (with-soft-assertions
        (expect (cdr (assoc :drawers included-options))
                :to-equal '(:include "LOGBOOK"))
        (expect (cdr (assoc :properties included-options))
                :to-equal '(:include "CUSTOM_ID" "UNUSED ITEM"))
        (expect included-drawers :to-equal '("LOGBOOK"))
        (expect (length included-property-drawers) :to-equal 1)
        (expect excluded-drawers :to-equal '("NOTES"))
        (expect (export-core-test-view-nodes no-matching-property :property-drawer)
                :to-be nil))))

  (it "prunes parsed clocks planning cookies inline tasks footnotes and selected properties"
    (let* ((source (export-core-test-policy-fixture-source "options-pruning.org"))
           (preparation-for
             (lambda (options)
               (export-core-test-preparation
                (format nil "#+OPTIONS: ~A~%~A" options source))))
           (visible (funcall preparation-for
                             "arch:t c:t p:t stat:t inline:t tex:t f:t d:t prop:t"))
           (pruned (funcall preparation-for
                            "arch:nil c:nil p:nil stat:nil inline:nil tex:nil f:nil d:(LOGBOOK) prop:nil"))
           (visible-drawers
             (mapcar (lambda (view)
                       (node-property (org-kit::export-view-node view) :drawer-name))
                     (export-core-test-view-nodes visible :drawer)))
           (pruned-drawers
             (mapcar (lambda (view)
                       (node-property (org-kit::export-view-node view) :drawer-name))
                     (export-core-test-view-nodes pruned :drawer))))
      (with-soft-assertions
        (dolist (type '(:clock :planning :statistics-cookie :inlinetask
                        :latex-fragment :latex-environment :footnote-reference
                        :footnote-definition :property-drawer))
          (expect (export-core-test-view-nodes visible type) :to-be-truthy)
          (expect (export-core-test-view-nodes pruned type) :to-be nil))
        (expect (member "LOGBOOK" visible-drawers :test #'string=) :to-be-truthy)
        (expect (member "NOTES" visible-drawers :test #'string=) :to-be-truthy)
        (expect pruned-drawers :to-equal '("LOGBOOK"))
        (expect (mapcar (lambda (view)
                          (node-property (org-kit::export-view-node view)
                                         :raw-value))
                        (export-core-test-view-nodes pruned :headline))
                :to-equal '("Task [0/1]" "Completed" "Normal")))))

  (it "applies supported TODO selection modes to parsed headings"
    (let* ((headings-for
             (lambda (options)
               (let ((preparation
                       (export-core-test-preparation-with-options options)))
                 (mapcar
                  (lambda (view)
                    (let ((node (org-kit::export-view-node view)))
                      (list (node-property node :raw-value)
                            (node-property node :todo-keyword)
                            (node-property node :todo-type))))
                  (export-core-test-view-nodes preparation :headline)))))
           (todo-included (funcall headings-for "tasks:(TODO)"))
           (done-selected (funcall headings-for "tasks:done")))
      (with-soft-assertions
        (expect todo-included
                :to-equal '(("Task [0/1]" "TODO" :todo)
                            ("Normal" nil nil)
                            ("Archived" nil nil)))
        (expect done-selected
                :to-equal '(("Completed" "DONE" :done)
                            ("Normal" nil nil)
                            ("Archived" nil nil))))))

  (it "accepts an explicit time context and rejects an invalid one"
    (let* ((time-context
             (make-time-context :reference-time (make-org-time-value "2030-01-02")
                                :zone (make-fixed-time-zone 0)))
           (context (make-export-context :time-context time-context))
           (invalid
             (export-core-test-catch-error
              (lambda () (make-export-context :time-context :local))))
           (invalid-export
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare (parse-org "Body") :html
                                          :context :invalid)))))
      (with-soft-assertions
        (expect (export-context-time-context context) :to-be time-context)
        (expect (typep invalid 'type-error) :to-be t)
        (expect (typep invalid-export 'type-error) :to-be t))))

  (it "prepares root body content when selection is inactive"
    (let* ((preparation (export-core-test-preparation "Body"))
           (paragraphs (export-core-test-view-nodes preparation :paragraph)))
      (expect (length paragraphs) :to-be 1)))

  (it "omits nodes mapped to the omit operation"
    (let ((preparation
            (export-core-test-preparation
             (export-core-test-lines
              "#+BEGIN_COMMENT" "hidden comment" "#+END_COMMENT" "Visible"))))
      (let ((comment-view
              (first (export-core-test-view-nodes preparation :comment-block))))
        (with-soft-assertions
          (expect (getf (org-kit::export-view-payload comment-view) :operation)
                  :to-be :omit)
          (expect (org-kit::export-view-children comment-view) :to-be nil)
          (expect (export-core-test-view-nodes preparation :paragraph)
                  :to-be-truthy)))))

  (it "applies explicit tag selection and exclusion before building the TOC"
    (let* ((source
             (export-core-test-lines
              "#+SELECT_TAGS: ship noexport"
              "#+EXCLUDE_TAGS: discard"
              "#+OPTIONS: toc:3 h:3"
              "* Container"
              "** Selected :ship:"
              "*** Nested"
              "** SelectedThenExcluded :ship:discard:"
              "** DefaultSelectTagIsReplaced :export:"
              "** ExplicitlySelectedNoexport :noexport:"))
           (preparation (export-core-test-preparation source))
           (headings (mapcar (lambda (view)
                               (node-property (org-kit::export-view-node view)
                                              :raw-value))
                             (export-core-test-view-nodes preparation :headline)))
           (toc (org-kit::export-preparation-toc preparation)))
      (with-soft-assertions
        (expect headings :to-equal
                '("Container" "Selected" "Nested" "ExplicitlySelectedNoexport"))
        (expect (mapcar (lambda (entry) (getf entry :title)) toc)
                :to-equal headings)
        (expect (mapcar (lambda (entry) (getf entry :level)) toc)
                :to-equal '(1 2 3 2)))))

  (it "skips keyword validation and include resolution in unselected subtrees"
    (let* ((source
             (export-core-test-lines
              "#+TITLE: Global title"
              "#+SELECT_TAGS: keep"
              "* Hidden :discard:"
              "#+INCLUDE: \"hidden.org\""
              "#+SETUPFILE: hidden.setup"
              "* Kept :keep:"
              "Visible"))
           (resolver-calls 0)
           (condition
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 source :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore path parent-source))
                   (incf resolver-calls)
                   "Included text")))))
           (preparation (and (null condition)
                             (export-core-test-preparation source)))
           (headings (and preparation
                          (mapcar (lambda (view)
                                    (node-property
                                     (org-kit::export-view-node view) :raw-value))
                                  (export-core-test-view-nodes
                                   preparation :headline)))))
      (with-soft-assertions
        (expect condition :to-be nil)
        (expect resolver-calls :to-be 0)
        (when preparation
          (expect (getf (org-kit::export-preparation-metadata preparation) :title)
                  :to-equal "Global title")
          (expect headings :to-equal '("Kept"))))))

  (it "keeps default selection and excludes noexport headings"
    (let* ((preparation
             (export-core-test-preparation
              (export-core-test-lines
               "* Normal"
               "* Export tag :export:"
               "* Hidden :noexport:")))
           (headings (mapcar (lambda (view)
                               (node-property (org-kit::export-view-node view)
                                              :raw-value))
                             (export-core-test-view-nodes preparation :headline))))
      (expect headings :to-equal '("Export tag"))))

  (it "does not retain source blocks outside a selected heading"
    (let* ((preparation
             (export-core-test-preparation
              (export-core-test-lines
               "#+SELECT_TAGS: keep"
               "#+BEGIN_SRC text"
               "root-secret"
               "#+END_SRC"
               "#+RESULTS:"
               ": root-result"
               "* Hidden :drop:"
               "#+BEGIN_SRC text"
               "hidden-secret"
               "#+END_SRC"
               "#+RESULTS:"
               ": hidden-result"
               "* Kept :keep:"
               "#+BEGIN_SRC text"
               "kept-code"
               "#+END_SRC")))
           (source-views (export-core-test-view-nodes preparation :src-block)))
      (expect (mapcar (lambda (view)
                        (node-property (org-kit::export-view-node view) :value))
                      source-views)
              :to-equal (list (format nil "kept-code~%")))))

  (it "collects footnotes after pruning, deduplicates references, and rejects bad references"
    (let* ((source
             (export-core-test-lines
              "#+SELECT_TAGS: keep"
              "* Kept :keep:"
              "Reference [fn:shared] and again [fn:shared]."
              "[fn:shared] Selected definition."
              "[fn:unused] Unreferenced definition."
              "* Pruned :drop:"
              "[fn:pruned] Definition outside selection."))
           (preparation (export-core-test-preparation source))
           (footnotes (org-kit::export-preparation-footnotes preparation))
           (unresolved
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  "#+SELECT_TAGS: keep"
                  "* Hidden :drop:"
                  "[fn:missing] Definition outside selection."
                  "* Kept :keep:"
                  "Reference [fn:missing].")))))
           (duplicate
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  "* Duplicate definitions"
                  "[fn:twice] First definition."
                  "[fn:twice] Second definition."))))))
      (with-soft-assertions
        (expect (length footnotes) :to-be 1)
        (expect (getf (first footnotes) :label) :to-equal "shared")
        (expect (getf (first footnotes) :number) :to-be 1)
        (expect (typep unresolved 'org-export-error) :to-be t)
        (when (typep unresolved 'org-export-error)
          (expect (org-export-error-reason unresolved) :to-be :unresolved-footnote))
        (expect (typep duplicate 'org-export-error) :to-be t)
        (when (typep duplicate 'org-export-error)
          (expect (org-export-error-reason duplicate)
                  :to-be :duplicate-footnote-definition)))))

  (it "keeps an anonymous inline footnote at its source position"
    (let* ((preparation (export-core-test-preparation "Inline [fn::note text]."))
           (footnote-views (export-core-test-view-nodes preparation
                                                        :footnote-reference)))
      (with-soft-assertions
        (expect (length footnote-views) :to-be 1)
        (expect (org-kit::export-preparation-footnotes preparation) :to-equal nil)
        (expect (node-property (org-kit::export-view-node (first footnote-views))
                               :type)
                :to-be :inline))))

  (it "falls back to the source node start when template text is unavailable"
    (let ((node (make-node :paragraph :begin 17 :end 24)))
      (with-soft-assertions
        (expect (org-kit::export-node-text-position node "missing") :to-be 17)
        (expect (org-kit::export-node-text-position nil "missing") :to-be 0))))

  (it "signals public export conditions with their backend, reason, and source"
    (let ((unknown-backend
            (export-core-test-catch-error
             (lambda ()
               (org-kit::export-prepare (parse-org "Body") :bogus))))
          (unsupported
            (export-core-test-catch-error
             (lambda ()
               (export-core-test-preparation "%%(org-calendar-holiday)")))))
      (with-soft-assertions
        (expect (typep unknown-backend 'org-export-error) :to-be t)
        (when (typep unknown-backend 'org-export-error)
          (expect (org-export-error-backend unknown-backend) :to-be :bogus)
          (expect (org-export-error-reason unknown-backend) :to-be :unknown-backend)
          (expect (org-export-error-source unknown-backend) :to-be nil))
        (expect (typep unsupported 'org-export-unsupported-syntax) :to-be t)
        (when (typep unsupported 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-backend unsupported) :to-be :html)
          (expect (org-export-unsupported-syntax-feature unsupported)
                  :to-be :diary-sexp)
          (expect (org-export-unsupported-syntax-position unsupported) :to-be 0)
          (expect (stringp (org-export-unsupported-syntax-source unsupported))
                  :to-be t)))))

  (it "maps every known node type and rejects unsupported or unknown syntax"
    (let* ((mapped-types (mapcar #'first org-kit::*export-node-operations*))
           (known-types (node-types))
           (unknown-keyword-source
             (export-core-test-lines "Body" "" "#+UNSUPPORTED: value"))
           (unsupported
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation "%%(org-calendar-holiday)"))))
           (unknown-keyword
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation unknown-keyword-source)))))
      (with-soft-assertions
        (expect (sort (copy-list mapped-types) #'string< :key #'symbol-name)
                :to-equal
                (sort (copy-list known-types) #'string< :key #'symbol-name))
        (expect (org-kit::export-node-operation :headline :html) :to-be :heading)
        (expect (org-kit::export-node-operation :clock :gfm) :to-be :omit)
        (expect (org-kit::export-node-operation :export-block :ascii) :to-be :literal)
        (expect (typep unsupported 'org-export-unsupported-syntax) :to-be t)
        (when (typep unsupported 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unsupported)
                  :to-be :diary-sexp))
        (expect (typep unknown-keyword 'org-export-unsupported-syntax) :to-be t)
        (when (typep unknown-keyword 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unknown-keyword)
                  :to-be :unknown-keyword)
          (expect (org-export-unsupported-syntax-position unknown-keyword)
                  :to-be (search "#+UNSUPPORTED" unknown-keyword-source))))))

  (it "bounds condition source excerpts"
    (let* ((source
             (coerce (list #\a (code-char 92) #\Newline #\Return #\Tab
                           (code-char 1) (code-char 128) (code-char #xe9))
                     'string))
           (expected-source
             (coerce (list #\a (code-char 92) (code-char 92) (code-char 92)
                           (char "n" 0) (code-char 92) (char "r" 0)
                           (code-char 92) (char "t" 0)
                           (code-char 92) (char "x" 0) #\0 #\1
                           (code-char 92) (char "x" 0) #\8 #\0 (code-char #xe9))
                     'string))
           (long-source (make-string (1+ org-kit::*condition-text-limit*)
                                     :initial-element #\x))
           (bounded (org-kit::export-bounded-source long-source))
           (missing-offset-source
             (let ((document (make-document ""))
                   (node (make-node :org-data)))
               (setf (document-root document) node)
               (org-kit::export-bounded-source node)))
           (long-node-condition
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (concatenate 'string "#+UNSUPPORTED: " long-source)))))
           (bounded-node-source
             (when (typep long-node-condition 'org-export-unsupported-syntax)
               (org-export-unsupported-syntax-source long-node-condition))))
      (with-soft-assertions
        (expect (org-kit::export-bounded-source source) :to-equal expected-source)
        (expect missing-offset-source :to-be nil)
        (expect (length bounded) :to-be (+ org-kit::*condition-text-limit* 3))
        (expect (subseq bounded 0 org-kit::*condition-text-limit*)
                :to-equal (subseq long-source 0 org-kit::*condition-text-limit*))
        (expect (subseq bounded (- (length bounded) 3)) :to-equal "...")
        (expect (typep long-node-condition 'org-export-unsupported-syntax)
                :to-be t)
        (expect (length bounded-node-source)
                :to-be (+ org-kit::*condition-text-limit* 3))
        (expect (subseq bounded-node-source 0 (length "#+UNSUPPORTED: "))
                :to-equal "#+UNSUPPORTED: "))))

  (it "validates export options, tags, and duplicate metadata"
    (let* ((preparation
             (export-core-test-preparation
              "#+OPTIONS: toc:-1 h:-2 num:3 expand-links:t creator:comment ^:nil title:t author:nil date:t"))
           (options (org-kit::export-preparation-options preparation))
           (option-failures
             (mapcar
              (lambda (source)
                (export-core-test-catch-error
                 (lambda () (export-core-test-preparation source))))
              '("#+OPTIONS: num:maybe"
                "#+OPTIONS: creator:author"
                "#+OPTIONS: toc:bad"
                "#+OPTIONS: h:x"
                "#+OPTIONS: unknown:t"
                "#+OPTIONS: toc"
                "#+OPTIONS: toc:"
                "#+OPTIONS: \"bad\""
                "#+OPTIONS: (TODO)"
                "#+OPTIONS: ^:maybe"
                "#+OPTIONS: arch:maybe"
                "#+OPTIONS: tex:maybe"
                "#+OPTIONS: tags:maybe"
                "#+OPTIONS: <:unknown"
                "#+OPTIONS: c:maybe"
                "#+OPTIONS: tasks:not"
                "#+OPTIONS: tasks:(not TODO)"
                "#+OPTIONS: prop:(not CUSTOM_ID)"
                "#+OPTIONS: d:LOGBOOK"
                "#+OPTIONS: d:(LOGBOOK"
                "#+OPTIONS: d:)"
                "#+OPTIONS: d:(bad!)"
                "#+OPTIONS: d:(\"LOGBOOK\"after)"
                "#+OPTIONS: d:(\"LOGBOOK)"
                "#+OPTIONS: broken-links:mark"
                "#+OPTIONS: tags:unknown")))
           (valid-tags
             (org-kit::export-parse-tag-list "ship-it_@#%" nil :html
                                             :select-tag-expression))
           (invalid-tags
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-parse-tag-list "bad:tag" nil :html
                                                :select-tag-expression))))
           (empty-macro-name
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation "#+MACRO:"))))
           (duplicate-title
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines "#+TITLE: first" "#+TITLE: second")))))
           (duplicate-macro
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  "#+MACRO: repeat first" "#+MACRO: repeat second"))))))
      (with-soft-assertions
        (expect (cdr (assoc :toc options)) :to-be -1)
        (expect (cdr (assoc :heading-depth options)) :to-be -2)
        (expect (cdr (assoc :num options)) :to-be 3)
        (expect (cdr (assoc :expand-links options)) :to-be t)
        (expect (cdr (assoc :creator options)) :to-be :comment)
        (expect (cdr (assoc :^ options)) :to-be nil)
        (expect (cdr (assoc :title options)) :to-be t)
        (expect (cdr (assoc :author options)) :to-be nil)
        (expect (cdr (assoc :date options)) :to-be t)
        (expect valid-tags :to-equal '("ship-it_@#%"))
        (expect (typep empty-macro-name 'org-export-unsupported-syntax)
                :to-be t)
        (expect (typep invalid-tags 'org-export-unsupported-syntax) :to-be t)
        (expect (every (lambda (condition)
                         (typep condition 'org-export-unsupported-syntax))
                       option-failures)
                :to-be t)
        (expect (typep duplicate-title 'org-export-unsupported-syntax) :to-be t)
        (when (typep duplicate-title 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature duplicate-title)
                  :to-be :duplicate-keyword))
        (expect (typep duplicate-macro 'org-export-unsupported-syntax) :to-be t)
        (when (typep duplicate-macro 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature duplicate-macro)
                  :to-be :duplicate-macro)))))

  (it "expands literal macros without evaluation and rejects recursion"
    (let* ((source
             (export-core-test-lines
              "#+MACRO: greeting Hello, $1! $2 / $1"
              "{{{greeting(World,again)}}}"))
           (preparation (export-core-test-preparation source))
           (macro-view (first (export-core-test-view-nodes preparation :macro)))
           (recursive
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  "#+MACRO: loop {{{loop}}}"
                  "{{{loop}}}")))))
           (unknown-macro
             (export-core-test-catch-error
              (lambda () (export-core-test-preparation "{{{missing}}}"))))
           (function-macro
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines "#+MACRO: choose(x) $1"
                                         "{{{choose(value)}}}")))))
           (invalid-macro-name
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation "#+MACRO: bad$ value"))))
           (unsupported-parameter
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  "#+MACRO: third $3"
                  "{{{third(value)}}}")))))
           (short-dollar-preparation
             (export-core-test-preparation
              (export-core-test-lines "#+MACRO: short $" "{{{short}}}")))
           (short-dollar-view
             (first (export-core-test-view-nodes short-dollar-preparation :macro)))
           (short-dollar (getf (org-kit::export-view-payload short-dollar-view)
                               :expanded-text))
           (nondigit-preparation
             (export-core-test-preparation
              (export-core-test-lines "#+MACRO: literal $x" "{{{literal}}}")))
           (nondigit-view
             (first (export-core-test-view-nodes nondigit-preparation :macro)))
           (nondigit-parameter (getf (org-kit::export-view-payload nondigit-view)
                                    :expanded-text))
           (multiple-digit-source
             (export-core-test-lines "#+MACRO: many $12" "{{{many}}}"))
           (multiple-digit-parameter
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation multiple-digit-source)))))
      (with-soft-assertions
        (expect (getf (org-kit::export-view-payload macro-view) :expanded-text)
                :to-equal "Hello, World! again / World")
        (expect (typep recursive 'org-export-unsupported-syntax) :to-be t)
        (when (typep recursive 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature recursive)
                  :to-be :recursive-macro))
        (expect (typep unknown-macro 'org-export-unsupported-syntax) :to-be t)
        (when (typep unknown-macro 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unknown-macro)
                  :to-be :unknown-macro))
        (expect (typep function-macro 'org-export-unsupported-syntax) :to-be t)
        (when (typep function-macro 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature function-macro)
                  :to-be :function-macro))
        (expect (typep invalid-macro-name 'org-export-unsupported-syntax)
                :to-be t)
        (when (typep invalid-macro-name 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature invalid-macro-name)
                  :to-be :invalid-macro))
        (expect (typep unsupported-parameter 'org-export-unsupported-syntax)
                :to-be t)
        (when (typep unsupported-parameter 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unsupported-parameter)
                  :to-be :unsupported-macro-parameter))
        (expect short-dollar :to-equal "$")
        (expect nondigit-parameter :to-equal "$x")
        (expect (typep multiple-digit-parameter 'org-export-unsupported-syntax)
                :to-be t)
        (when (typep multiple-digit-parameter 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature multiple-digit-parameter)
                  :to-be :unsupported-macro-parameter)
          (expect (org-export-unsupported-syntax-position multiple-digit-parameter)
                  :to-be (search "$12" multiple-digit-source))))))

  (it "rejects long numeric options and macro parameters"
    (let* ((digits (make-string 4096 :initial-element #\9))
           (option-error
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (concatenate 'string "#+OPTIONS: toc:" digits)))))
           (macro-error
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (export-core-test-lines
                  (concatenate 'string "#+MACRO: many $" digits)
                  "{{{many}}}"))))))
      (with-soft-assertions
        (expect (typep option-error 'org-export-unsupported-syntax) :to-be t)
        (when (typep option-error 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature option-error)
                  :to-be :invalid-option))
        (expect (typep macro-error 'org-export-unsupported-syntax) :to-be t)
        (when (typep macro-error 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature macro-error)
                  :to-be :unsupported-macro-parameter)))))

  (it "uses only the include callback and rejects missing, invalid, or cyclic includes"
    (let* ((source (parse-org "#+INCLUDE: \"part.org\"" :source-name "main.org"))
           (calls nil)
           (resolver
             (lambda (path source-name)
               (push (list path source-name) calls)
               (when (string= path "part.org")
                 (export-core-test-lines "* Included" "Included text"))))
           (preparation (org-kit::export-prepare source :html
                                                 :include-resolver resolver))
           (included-headings
             (mapcar (lambda (view)
                       (node-property (org-kit::export-view-node view) :raw-value))
                     (export-core-test-view-nodes preparation :headline)))
           (missing (export-core-test-catch-error
                     (lambda () (org-kit::export-prepare source :html))))
           (invalid (export-core-test-catch-error
                     (lambda ()
                       (org-kit::export-prepare source :html
                                                :include-resolver
                                                (lambda (path source-name)
                                                  (declare (ignore path source-name))
                                                  nil)))))
           (cycle-source (parse-org "#+INCLUDE: \"part.org\""
                                    :source-name "main.org"))
           (cycle (export-core-test-catch-error
                   (lambda ()
                     (org-kit::export-prepare
                      cycle-source :html
                      :include-resolver
                      (lambda (path source-name)
                        (declare (ignore source-name))
                        (if (string= path "part.org")
                            "#+INCLUDE: \"part.org\""
                            "unreachable")))))))
      (with-soft-assertions
        (expect calls :to-equal '(("part.org" "main.org")))
        (expect included-headings :to-equal '("Included"))
        (expect (typep missing 'org-export-error) :to-be t)
        (when (typep missing 'org-export-error)
          (expect (org-export-error-reason missing) :to-be :missing-include-resolver))
        (expect (typep invalid 'org-export-error) :to-be t)
        (when (typep invalid 'org-export-error)
          (expect (org-export-error-reason invalid) :to-be :invalid-include-result))
        (expect (typep cycle 'org-export-error) :to-be t)
        (when (typep cycle 'org-export-error)
          (expect (org-export-error-reason cycle) :to-be :include-cycle)))))

  (it "isolates include callback arguments from document and cycle state"
    (let* ((source-name (copy-seq "main.org"))
           (document (parse-org "#+INCLUDE: \"part.org\""
                                :source-name source-name))
           (calls 0)
           (condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 document :html
                 :include-resolver
                 (lambda (path parent-source)
                   (incf calls)
                   (setf (char path 0) #\P
                         (char parent-source 0) #\M)
                   "#+INCLUDE: \"part.org\""))))))
      (with-soft-assertions
        (expect calls :to-be 1)
        (expect source-name :to-equal "main.org")
        (expect (document-source-name document) :to-equal "main.org")
        (expect (typep condition 'org-export-error) :to-be t)
        (when (typep condition 'org-export-error)
          (expect (org-export-error-reason condition) :to-be :include-cycle)))))

  (it "validates include forms and rejects fragment-scoped settings"
    (let* ((invalid-include-forms
             (mapcar
              (lambda (value)
                (export-core-test-catch-error
                 (lambda ()
                   (export-core-test-preparation
                    (format nil "#+INCLUDE: ~A" value)))))
              (list "" "part.org" "\"part.org" "part.org\"" "\"a\"b\""
                    (format nil "\"bad~%path\"")
                    (format nil "\"bad~Cpath\"" #\Tab))))
           (fragment-errors
             (mapcar
              (lambda (fragment)
                (export-core-test-catch-error
                 (lambda ()
                   (org-kit::export-prepare
                    (parse-org "#+INCLUDE: \"part.org\"") :html
                    :include-resolver (lambda (path parent)
                                        (declare (ignore path parent))
                                        fragment)))))
              '("#+TITLE: included title" "#+TODO: TODO DONE"))))
      (with-soft-assertions
        (expect (every (lambda (condition)
                         (typep condition 'org-export-unsupported-syntax))
                       invalid-include-forms)
                :to-be t)
        (expect (every (lambda (condition)
                         (and (typep condition 'org-export-unsupported-syntax)
                              (eq (org-export-unsupported-syntax-feature condition)
                                  :fragment-keyword-scope)))
                       fragment-errors)
                :to-be t))))

  (it "passes a NIL parent source name to include resolvers"
    (let* ((document (parse-org "#+INCLUDE: \"part.org\""))
           (parent-source :not-called)
           (preparation
             (org-kit::export-prepare
              document :html
              :include-resolver (lambda (path source-name)
                                  (declare (ignore path))
                                  (setf parent-source source-name)
                                  "Included"))))
      (with-soft-assertions
        (expect parent-source :to-be nil)
        (expect (org-kit::export-preparation-p preparation) :to-be t))))

  (it "copies string source names before passing them to include resolvers"
    (let* ((source-name (copy-seq "main.org"))
           (document (parse-org "#+INCLUDE: \"part.org\""
                                :source-name source-name))
           (parent-source nil)
           (preparation
             (org-kit::export-prepare
              document :html
              :include-resolver (lambda (path source)
                                  (declare (ignore path))
                                  (setf parent-source source
                                        (char source 0) #\M)
                                  "Included"))))
      (with-soft-assertions
        (expect parent-source :to-equal "Main.org")
        (expect source-name :to-equal "main.org")
        (expect (document-source-name document) :to-equal "main.org")
        (expect (org-kit::export-preparation-p preparation) :to-be t))))

  (it "rejects non-string source names before invoking include resolvers"
    (let* ((source-name (make-hash-table))
           (document (parse-org "#+INCLUDE: \"part.org\""
                                :source-name source-name))
           (calls 0)
           (condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 document :html
                 :include-resolver (lambda (path parent-source)
                                     (declare (ignore path parent-source))
                                     (incf calls)
                                     "Included"))))))
      (with-soft-assertions
        (expect calls :to-be 0)
        (expect (typep condition 'type-error) :to-be t))))

  (it "passes each nested include the path of its parent source"
    (let* ((document (parse-org "#+INCLUDE: \"outer.org\""
                                :source-name "main.org"))
           (calls nil)
           (preparation
             (org-kit::export-prepare
              document :html
              :include-resolver
              (lambda (path parent-source)
                (push (list path parent-source) calls)
                (if (string= path "outer.org")
                    "#+INCLUDE: \"inner.org\""
                    "Included")))))
      (with-soft-assertions
        (expect (nreverse calls)
                :to-equal '(("outer.org" "main.org")
                            ("inner.org" "outer.org")))
        (expect (org-kit::export-preparation-p preparation) :to-be t))))

  (it "interprets only source block exports and validates their values"
    (let* ((with-ignored-arguments
             (export-core-test-preparation
              (export-core-test-lines
               "#+BEGIN_SRC text x plain :results output :tangle file :var x=1 :other :empty x :exports both"
               "body" "#+END_SRC" "#+RESULTS:" ": result")))
           (with-ignored-view
             (first (export-core-test-view-nodes with-ignored-arguments :src-block)))
           (ignored-arguments
             (export-core-test-preparation
              (export-core-test-lines
               "#+BEGIN_SRC text :results output :tangle file :var x=1"
               "body" "#+END_SRC")))
           (ignored-view
             (first (export-core-test-view-nodes ignored-arguments :src-block)))
           (invalid-headers
             (mapcar
              (lambda (header)
                (export-core-test-catch-error
                 (lambda ()
                   (export-core-test-preparation
                    (export-core-test-lines
                     (concatenate 'string "#+BEGIN_SRC text " header)
                     "body" "#+END_SRC")))))
              '(":exports code :exports results" ":exports" ":exports never"))))
      (with-soft-assertions
        (expect (getf (org-kit::export-view-payload with-ignored-view) :exports)
                :to-be :both)
        (expect (getf (org-kit::export-view-payload with-ignored-view)
                      :requested-exports)
                :to-be :both)
        (expect (getf (org-kit::export-view-payload with-ignored-view)
                      :adjacent-results)
                :to-be t)
        (expect (getf (org-kit::export-view-payload ignored-view) :exports)
                :to-be :both)
        (expect (getf (org-kit::export-view-payload ignored-view)
                      :requested-exports)
                :to-be :code)
        (expect (every (lambda (condition)
                         (and (typep condition 'org-export-unsupported-syntax)
                              (eq (org-export-unsupported-syntax-feature condition)
                                  :src-header-argument)))
                       invalid-headers)
                :to-be t))))

  (it "prepares code and adjacent results according to every source export mode"
    (let* ((source
             (export-core-test-lines
              "#+BEGIN_SRC text :exports both"
              "both-code"
              "#+END_SRC"
              "#+RESULTS:"
              ": both-result"
              "#+BEGIN_SRC text :exports code"
              "code-code"
              "#+END_SRC"
              "#+RESULTS:"
              ": code-result"
              "#+BEGIN_SRC text :exports results"
              "results-code"
              "#+END_SRC"
              "#+RESULTS:"
              ": results-result"
              "#+BEGIN_SRC text :exports none"
              "none-code"
              "#+END_SRC"
              ""
              "#+RESULTS:"
              ": none-result"
              "#+BEGIN_SRC text"
              "default-code"
              "#+END_SRC"
              "#+BEGIN_SRC text :exports results"
              "separated-code"
              "#+END_SRC"
              "The paragraph separates the following results from the source block."
              ""
              "#+RESULTS:"
              ": separated-result"))
           (preparation (export-core-test-preparation source))
           (source-views (export-core-test-view-nodes preparation :src-block))
           (source-payloads (mapcar #'org-kit::export-view-payload source-views))
           (result-child-counts
             (mapcar (lambda (view)
                       (length (org-kit::export-view-children view)))
                     source-views))
           (separated-result-view
             (find-if (lambda (view)
                        (let ((node (org-kit::export-view-node view)))
                          (and (eq (node-type node) :fixed-width)
                               (node-property node :RESULTS)
                               (search "separated-result"
                                       (node-property node :value)))))
                      (export-core-test-all-views preparation)))
           (separated-result
             (and separated-result-view
                  (org-kit::export-view-node separated-result-view))))
      (with-soft-assertions
        (expect (mapcar (lambda (payload) (getf payload :exports)) source-payloads)
                :to-equal '(:both :both :both :both :both :both))
        (expect (mapcar (lambda (payload)
                          (getf payload :requested-exports))
                        source-payloads)
                :to-equal '(:both :code :results :none :code :results))
        (expect (mapcar (lambda (payload) (getf payload :adjacent-results))
                        source-payloads)
                :to-equal '(t t t t nil nil))
        (expect result-child-counts :to-equal '(1 1 1 1 0 0))
        (expect separated-result :to-be-truthy)
        (expect (and separated-result-view
                     (eq (getf (org-kit::export-view-payload separated-result-view)
                               :operation)
                         :code-block))
                :to-be t)
        (expect (and separated-result
                     (string= (node-property separated-result :value)
                              "separated-result"))
                :to-be t))))

  (it "does not mutate documents on successful preparation or failure"
    (let* ((source (parse-org "#+INCLUDE: \"part.org\"" :source-name "main.org"))
           (before (export-core-test-document-snapshot source)))
      (org-kit::export-prepare source :html
                               :include-resolver
                               (lambda (path source-name)
                                 (declare (ignore source-name))
                                 (if (string= path "part.org") "Included" nil)))
      (with-soft-assertions
        (expect (export-core-test-document-snapshot source) :to-equal before)
        (expect (typep (export-core-test-catch-error
                        (lambda () (org-kit::export-prepare source :html)))
                       'org-export-error)
                :to-be t)
        (expect (export-core-test-document-snapshot source) :to-equal before))))

  (it "validates preparation inputs and requires a parsed document"
    (let* ((document (parse-org "Body"))
           (invalid-document
             (export-core-test-catch-error
              (lambda () (org-kit::export-prepare nil :html))))
           (invalid-backend
             (export-core-test-catch-error
              (lambda () (org-kit::export-prepare document :unknown))))
           (invalid-resolver
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare document :html :include-resolver :invalid))))
           (invalid-raw-html
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare document :html :allow-raw-html :invalid))))
           (unparsed
             (export-core-test-catch-error
              (lambda () (org-kit::export-prepare (make-document "Body") :html)))))
      (with-soft-assertions
        (expect (typep invalid-document 'type-error) :to-be t)
        (expect (typep invalid-backend 'org-export-error) :to-be t)
        (when (typep invalid-backend 'org-export-error)
          (expect (org-export-error-reason invalid-backend) :to-be :unknown-backend))
        (expect (typep invalid-resolver 'type-error) :to-be t)
        (expect (typep invalid-raw-html 'type-error) :to-be t)
        (expect (typep unparsed 'type-error) :to-be t)))))

(describe "E1 export resource boundaries"
  (it "bounds preparation depth across included fragments"
    (let* ((fragments
             (loop for index from 1 to 32
                   collect
                   (with-output-to-string (output)
                     (loop for level from 1 to 12
                           do (format output "~A Heading ~D~%Content ~D~%"
                                      (make-string level :initial-element #\*)
                                      level level))
                     (format output "#+INCLUDE: \"part-~D.org\""
                             (1+ index)))))
           (document
             (parse-org "#+INCLUDE: \"part-1.org\""
                        :source-name "main.org"
                        :limits (make-limits :max-element-depth 128)))
           (condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 document :html
                 :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore parent-source))
                   (let* ((number (parse-integer path :start 5
                                                 :end (- (length path) 4)))
                          (fragment (nth (1- number) fragments)))
                     fragment)))))))
      (expect (typep condition 'org-resource-limit-error) :to-be t)
      (when (typep condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name condition)
                :to-be :max-element-depth)
        (expect (org-resource-limit-error-limit condition) :to-be 128)
        (expect (> (org-resource-limit-error-actual condition) 128) :to-be t))
      (expect (typep condition 'storage-condition) :to-be nil)))

  (it "checks root nodes before processing export keywords"
    (let* ((document
             (parse-org "#+UNSUPPORTED: value"
                        :limits (make-limits :max-export-expanded-nodes 0)))
           (condition
             (export-core-test-catch-error
              (lambda () (org-kit::export-prepare document :html)))))
      (expect (typep condition 'org-resource-limit-error) :to-be t)
      (when (typep condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name condition)
                :to-be :max-export-expanded-nodes)
        (expect (org-resource-limit-error-limit condition) :to-be 0)
        (expect (org-resource-limit-error-actual condition)
                :to-be (org-kit::export-node-count (document-root document))))))

  (it "preserves the parser node limit when a fragment exceeds it"
    (let* ((document (parse-org "#+INCLUDE: \"part.org\""
                                :limits (make-limits :max-node-count 4)))
           (condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 document :html
                 :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore path parent-source))
                   (export-core-test-lines
                    "* One" "* Two" "* Three" "* Four" "* Five")))))))
      (expect (typep condition 'org-resource-limit-error) :to-be t)
      (when (typep condition 'org-resource-limit-error)
        (expect (org-resource-limit-error-limit-name condition)
                :to-be :max-node-count))))

  (it "checks expansion depth immediately below, at, and above the limit"
    (let* ((source (export-core-test-lines "#+MACRO: m expansion" "{{{m}}}"))
           (below (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expansion-depth 0))))
           (at (export-core-test-catch-error
                (lambda ()
                  (export-core-test-prepare-with-limit
                   source :max-export-expansion-depth 1))))
           (above (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expansion-depth 2)))))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-export-expansion-depth)
          (expect (org-resource-limit-error-limit below) :to-be 0)
          (expect (org-resource-limit-error-actual below) :to-be 1))
        (expect at :to-be nil)
        (expect above :to-be nil))))

  (it "checks expanded characters immediately below, at, and above the limit"
    (let* ((source (export-core-test-lines "#+MACRO: m abc" "{{{m}}}"))
           (below (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expanded-characters 2))))
           (at (export-core-test-catch-error
                (lambda ()
                  (export-core-test-prepare-with-limit
                   source :max-export-expanded-characters 3))))
           (above (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expanded-characters 4)))))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-export-expanded-characters)
          (expect (org-resource-limit-error-limit below) :to-be 2)
          (expect (org-resource-limit-error-actual below) :to-be 3))
        (expect at :to-be nil)
        (expect above :to-be nil))))

  (it "checks cumulative expanded nodes immediately below, at, and above the limit"
    (let* ((source (export-core-test-lines "#+MACRO: m expanded" "{{{m}}}"))
           (document (parse-org source))
           (fragment (parse-org "expanded"))
           (actual (+ (org-kit::export-node-count (document-root document))
                      (org-kit::export-node-count (document-root fragment))))
           (below (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expanded-nodes (1- actual)))))
           (at (export-core-test-catch-error
                (lambda ()
                  (export-core-test-prepare-with-limit
                   source :max-export-expanded-nodes actual))))
           (above (export-core-test-catch-error
                   (lambda ()
                     (export-core-test-prepare-with-limit
                      source :max-export-expanded-nodes (1+ actual))))))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-export-expanded-nodes)
          (expect (org-resource-limit-error-limit below) :to-be (1- actual))
          (expect (org-resource-limit-error-actual below) :to-be actual))
        (expect at :to-be nil)
        (expect above :to-be nil)))))

(describe "E1 export output validation"
  (it "validates output types and streams through export-org"
    (let* ((source (parse-org "A é € 😀"))
           (expected (export-org source :html))
           (string-stream (make-string-output-stream))
           (string-stream-error
             (export-core-test-catch-error
              (lambda ()
                (export-org source :html :output-type :string
                            :stream string-stream))))
           (octets-stream-error
             (export-core-test-catch-error
              (lambda ()
                (export-org source :html :output-type :octets
                            :stream string-stream))))
           (missing-stream
             (export-core-test-catch-error
              (lambda ()
                (export-org source :html :output-type :stream))))
           (non-stream-error
             (export-core-test-catch-error
              (lambda ()
                (export-org source :html :output-type :stream :stream :invalid))))
           (input-stream
             (export-core-test-catch-error
              (lambda ()
                (export-org source :html :output-type :stream
                            :stream (make-string-input-stream "text")))))
           (unsupported-stream
             (export-core-test-catch-error
              (lambda ()
                (export-org source :html :output-type :stream
                            :stream
                            (make-instance 'export-core-test-binary-output-stream
                                           :element-type '(unsigned-byte 16))))))
           (unknown-output
             (export-core-test-catch-error
              (lambda ()
                (export-org source :html :output-type :unknown))))
           (surrogate (or (code-char #xd800)
                          (error "SBCL did not provide the surrogate test character")))
           (surrogate-document
             (let* ((document
                      (org-kit::%make-document :text (string surrogate)
                                               :line-starts #(0)
                                               :limits (make-limits)))
                    (text-node (make-node :plain-text :begin 0 :end 1))
                    (paragraph (make-node :paragraph :begin 0 :end 1
                                          :children (list text-node)))
                    (root (make-node :org-data :begin 0 :end 1
                                     :children (list paragraph))))
               (setf (document-root document) root)
               document))
           (unencodable
             (export-core-test-catch-error
              (lambda ()
                (export-org surrogate-document :html :output-type :octets))))
           (character-stream (make-string-output-stream))
           (character-result
             (export-org source :html :output-type :stream
                         :stream character-stream))
           (octets (export-org source :html :output-type :octets))
           (octet-stream
             (make-instance 'export-core-test-binary-output-stream
                            :element-type '(unsigned-byte 8)))
           (octet-result
             (export-org source :html :output-type :stream :stream octet-stream)))
      (with-soft-assertions
        (dolist (condition (list string-stream-error octets-stream-error
                                 missing-stream non-stream-error input-stream
                                 unsupported-stream unknown-output))
          (expect (typep condition 'org-export-error) :to-be t)
          (when (typep condition 'org-export-error)
            (expect (org-kit-error-message condition) :to-be-truthy)
            (expect (org-export-error-backend condition) :to-be :html)
            (expect (org-export-error-reason condition) :to-be :invalid-output)
            (expect (org-export-error-source condition) :to-be nil)))
        (expect (typep unencodable 'org-export-error) :to-be t)
        (when (typep unencodable 'org-export-error)
          (expect (org-kit-error-message unencodable) :to-be-truthy)
          (expect (org-export-error-backend unencodable) :to-be :html)
          (expect (org-export-error-reason unencodable) :to-be :encoding)
          (expect (org-export-error-source unencodable)
                  :to-equal (org-kit::export-bounded-source
                             (export-org surrogate-document :html))))
        (expect (get-output-stream-string string-stream) :to-equal "")
        (expect (eq character-result character-stream) :to-be t)
        (expect (get-output-stream-string character-stream) :to-equal expected)
        (expect octet-result :to-be octet-stream)
        (expect (export-core-test-stream-octets octet-stream) :to-equalp octets)
        (expect (search #(195 169) octets) :not :to-be nil)
        (expect (search #(226 130 172) octets) :not :to-be nil)
        (expect (search #(240 159 152 128) octets) :not :to-be nil))))

  (it "checks rendered character limits through export-org"
    (let* ((source "Body")
           (expected (export-org (parse-org source) :html))
           (length (length expected))
           (below
             (export-core-test-catch-error
              (lambda ()
                (export-org
                 (parse-org source :limits
                            (make-limits :max-output-length (1- length)))
                 :html))))
           (at (export-org (parse-org source :limits
                                      (make-limits :max-output-length length))
                           :html))
           (above (export-org
                   (parse-org source :limits
                              (make-limits :max-output-length (1+ length)))
                   :html)))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit below) :to-be (1- length))
          (expect (> (org-resource-limit-error-actual below) (1- length))
                  :to-be t))
        (expect at :to-equal expected)
        (expect above :to-equal expected))))

  (it "checks UTF-8 output octet limits through export-org"
    (let* ((source "é")
           (expected (export-org (parse-org source) :html :output-type :octets))
           (length (length expected))
           (below
             (export-core-test-catch-error
              (lambda ()
                (export-org
                 (parse-org source :limits
                            (make-limits :max-output-octets (1- length)))
                 :html :output-type :octets))))
           (at (export-org (parse-org source :limits
                                      (make-limits :max-output-octets length))
                           :html :output-type :octets))
           (above (export-org
                   (parse-org source :limits
                              (make-limits :max-output-octets (1+ length)))
                   :html :output-type :octets)))
      (with-soft-assertions
        (expect (typep below 'org-resource-limit-error) :to-be t)
        (when (typep below 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below)
                  :to-be :max-output-octets)
          (expect (org-resource-limit-error-limit below) :to-be (1- length))
          (expect (org-resource-limit-error-actual below) :to-be length))
        (expect at :to-equalp expected)
        (expect above :to-equalp expected)))))

(describe "E1 export fragment limits"
  (it "preserves configured parser limits through the public export API"
    (let* ((document
             (parse-org "#+INCLUDE: \"part.org\""
                        :limits (make-limits :max-heading-depth 2)))
           (condition
             (export-core-test-catch-error
              (lambda ()
                (export-org
                 document :html
                 :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore path parent-source))
                   (format nil "* One~%** Two~%*** Three")))))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when (typep condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-heading-depth)
          (expect (org-resource-limit-error-limit condition) :to-be 2)
          (expect (org-resource-limit-error-actual condition) :to-be 3))))))

(describe "E3 shared URL and renderer dispatch"
  (it "parses ASCII URL scheme characters and rejects malformed schemes"
    (with-soft-assertions
      (dolist (scheme '("h1" "h+" "h." "h-"))
        (expect (org-kit::export-explicit-url-scheme
                 (concatenate 'string scheme ":value"))
                :to-equal scheme))
      (dolist (destination '("relative/path" ":value" "1http:value"
                             "h_:value"))
        (expect (org-kit::export-explicit-url-scheme destination)
                :to-be nil))))

  (it "validates raw and expanded URLs before rendering"
    (let ((control (string (code-char 1)))
          (unicode-space (string (code-char #x3000))))
      (with-soft-assertions
        (expect (org-kit::export-safe-url-p "https://example.test"
                                             "https://example.test")
                :to-be t)
        (expect (org-kit::export-safe-url-p "guide/page" "guide/page")
                :to-be t)
        (expect (org-kit::export-safe-url-p "/" "/") :to-be t)
        (expect (org-kit::export-safe-url-p "javascript:alert(1)"
                                             "javascript:alert(1)")
                :to-be nil)
        (expect (org-kit::export-safe-url-p (concatenate 'string "https://x/"
                                                         control "raw")
                                             "https://example.test")
                :to-be nil)
        (expect (org-kit::export-safe-url-p "https://example.test"
                                             (concatenate 'string "https://x/"
                                                          control "expanded"))
                :to-be nil)
        (expect (org-kit::export-safe-url-p "" "") :to-be nil)
        (expect (org-kit::export-safe-url-p "guide" unicode-space)
                :to-be nil)
        (expect (org-kit::export-safe-url-p "guide\\private"
                                             "guide\\private")
                :to-be nil)
        (expect (org-kit::export-safe-url-p "//example.test" "//example.test")
                :to-be nil))))

  (it "accepts an explicit context through the public export API"
    (let* ((document (parse-org "Body"))
           (context (make-export-context :language "fr"))
           (output (export-org document :gfm :context context)))
      (with-soft-assertions
        (expect (export-context-p context) :to-be t)
        (expect (search "Body" output) :to-be-truthy))))

  (it "signals when a backend has no renderer method"
    (let* ((preparation (org-kit::export-prepare (parse-org "Body") :html))
           (condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-render-preparation preparation :unknown)))))
      (with-soft-assertions
        (expect (typep condition 'org-export-error) :to-be t)
        (when (typep condition 'org-export-error)
          (expect (org-export-error-backend condition) :to-be :unknown)
          (expect (org-export-error-reason condition)
                  :to-be :unsupported-backend))))))
