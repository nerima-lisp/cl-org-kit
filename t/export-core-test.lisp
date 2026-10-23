;;;; t/export-core-test.lisp
(in-package #:cl-org-kit/test)

(eval-when (:load-toplevel :execute)
  (load (asdf:system-relative-pathname
         "cl-org-kit" "t/fixtures/export-baseline/fr-106/baseline.lisp")))

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

(describe "FR-074 SETUPFILE expansion"
  (it "FR-074 matches nested keyword order and effective settings"
    (let* ((document
             (parse-org
              (export-core-test-lines
               "#+SETUPFILE: \"setup/root.org\""
               "#+TITLE: Main title"
               "#+OPTIONS: toc:2 num:nil"
               "#+LANGUAGE: ja"
               ""
               "Main body.")
              :source-name "manual/main.org"))
           (calls nil)
           (resolver
             (lambda (path parent-source)
               (push (list path parent-source) calls)
               (cond
                 ((string= path "setup/root.org")
                  (export-core-test-lines
                   "#+TITLE: Root title"
                   "#+OPTIONS: toc:1"
                   "#+SETUPFILE: nested/theme.org"
                   "#+OPTIONS: toc:3"
                   "#+LANGUAGE: en"
                   ""
                   "Root setup body."))
                 ((string= path "nested/theme.org")
                  (export-core-test-lines
                   "#+TITLE: Theme title"
                   "#+OPTIONS: toc:nil"
                   "#+LANGUAGE: fr"
                   ""
                   "Theme setup body."))
                 (t nil))))
           (keyword-values
             (multiple-value-bind (records node-count character-count)
                 (org-kit::export-setupfile-keyword-records
                  document :html resolver (document-limits document)
                  (org-kit::export-node-count (document-root document)) 0)
               (declare (ignore node-count character-count))
               (setf calls nil)
               (mapcar
                (lambda (record) (list (second record) (third record)))
                (remove-if
                 (lambda (record) (string= (second record) "SETUPFILE"))
                 records))))
           (preparation
             (org-kit::export-prepare document :html :include-resolver resolver))
           (metadata (org-kit::export-preparation-metadata preparation))
           (options (org-kit::export-preparation-options preparation))
           (paragraph-texts
             (mapcar (lambda (view)
                       (node-text (org-kit::export-view-node view)))
                     (export-core-test-view-nodes preparation :paragraph))))
      (with-soft-assertions
        (expect (nreverse calls)
                :to-equal (list (list "setup/root.org" "manual/main.org")
                                (list "nested/theme.org" "setup/root.org")))
        (expect keyword-values
                :to-equal
                (list (list "TITLE" "Root title")
                      (list "OPTIONS" "toc:1")
                      (list "TITLE" "Theme title")
                      (list "OPTIONS" "toc:nil")
                      (list "LANGUAGE" "fr")
                      (list "OPTIONS" "toc:3")
                      (list "LANGUAGE" "en")
                      (list "TITLE" "Main title")
                      (list "OPTIONS" "toc:2 num:nil")
                      (list "LANGUAGE" "ja")))
        (expect (getf metadata :title) :to-equal "Root title Theme title Main title")
        (expect (getf metadata :language) :to-equal "ja")
        (expect (cdr (assoc :toc options)) :to-equal 2)
        (expect (assoc :num options) :to-be-truthy)
        (expect (cdr (assoc :num options)) :to-be nil)
        (expect paragraph-texts :to-equal (list "Main body."))))))
(describe "FR-074 SETUPFILE condition handling" (it "FR-074 reports missing and invalid resolver conditions" (let* ((missing (export-core-test-catch-error (lambda () (export-core-test-preparation "#+SETUPFILE: a.org")))) (invalid (export-core-test-catch-error (lambda () (export-core-test-preparation "#+SETUPFILE: a.org" :include-resolver (lambda (path parent-source) (declare (ignore path parent-source)) nil)))))) (with-soft-assertions (expect (typep missing (intern "ORG-EXPORT-ERROR" "ORG-KIT")) :to-be t) (when (typep missing (intern "ORG-EXPORT-ERROR" "ORG-KIT")) (expect (org-export-error-reason missing) :to-be :missing-setupfile-resolver)) (expect (typep invalid (intern "ORG-EXPORT-ERROR" "ORG-KIT")) :to-be t) (when (typep invalid (intern "ORG-EXPORT-ERROR" "ORG-KIT")) (expect (org-export-error-reason invalid) :to-be :invalid-setupfile-result))))))
(describe "FR-074 SETUPFILE cycle and form handling"
  (it "FR-074 reports cycles and malformed paths"
    (let* ((cycle
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 (parse-org "#+SETUPFILE: a.org" :source-name "main.org") :html
                 :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore parent-source))
                   (if (string= path "a.org")
                       "#+SETUPFILE: a.org"
                       "unreachable"))))))
           (malformed
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation "#+SETUPFILE: \"unterminated")))))
      (with-soft-assertions
        (expect (typep cycle (intern "ORG-EXPORT-ERROR" "ORG-KIT")) :to-be t)
        (when (typep cycle (intern "ORG-EXPORT-ERROR" "ORG-KIT"))
          (expect (org-export-error-reason cycle) :to-be :setupfile-cycle))
        (expect (typep malformed
                       (intern "ORG-EXPORT-UNSUPPORTED-SYNTAX" "ORG-KIT"))
                :to-be t)
        (when (typep malformed
                     (intern "ORG-EXPORT-UNSUPPORTED-SYNTAX" "ORG-KIT"))
          (expect (org-export-unsupported-syntax-feature malformed)
                  :to-be :setupfile-form))))))
(describe "FR-074 SETUPFILE depth handling" (it "FR-074 applies the expansion depth limit" (let* ((depth (export-core-test-catch-error (lambda () (org-kit::export-prepare (parse-org "#+SETUPFILE: a.org" :source-name "main.org" :limits (make-limits :max-export-expansion-depth 1)) :html :include-resolver (lambda (path parent-source) (declare (ignore parent-source)) (if (string= path "a.org") "#+SETUPFILE: b.org" "#+TITLE: Beyond limit"))))))) (with-soft-assertions (expect (typep depth (intern "ORG-RESOURCE-LIMIT-ERROR" "ORG-KIT")) :to-be t) (when (typep depth (intern "ORG-RESOURCE-LIMIT-ERROR" "ORG-KIT")) (expect (org-resource-limit-error-limit-name depth) :to-be :max-export-expansion-depth) (expect (org-resource-limit-error-actual depth) :to-be 2))))))

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
                   "recursive-macro.org")))
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
            (if (string= name "recursive-macro.org")
                (progn
                  (expect (typep condition 'org-export-unsupported-syntax)
                          :to-be t)
                  (when (typep condition 'org-export-unsupported-syntax)
                    (expect (org-export-unsupported-syntax-feature condition)
                            :to-be :recursive-macro)))
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

  (it "prunes children for common node policies"
    (let ((preparation
            (export-core-test-preparation
             (export-core-test-lines
              "#+BEGIN_COMMENT" "hidden comment" "#+END_COMMENT" "Visible"))))
      (let ((comment-view
              (first (export-core-test-view-nodes preparation :comment-block))))
        (with-soft-assertions
          (expect (org-kit::export-view-children comment-view) :to-be nil)
          (expect (export-core-test-view-nodes preparation :paragraph)
                  :to-be-truthy)))))

  (it "applies prune-children to special preparation paths"
    (let ((backend :api-export-special-node-pruning)
          (registered-p nil)
          (include-calls 0))
      (unwind-protect
           (progn
             (register-export-backend
              backend :renderer #'identity
              :node-policies
              (list (cons :headline :prune-children)
                    (cons :keyword :prune-children)
                    (cons :macro :prune-children)
                    (cons :src-block :prune-children)))
             (setf registered-p t)
             (let* ((source
                      (export-core-test-lines
                       "{{{unknown-body-macro}}}"
                       "#+INCLUDE: \"child.org\""
                       "#+BEGIN_SRC text :exports both"
                       "source text"
                       "#+END_SRC"
                       "#+RESULTS:" ": result text"
                       "* {{{unknown-title-macro}}}"
                       "Child paragraph."))
                    (preparation
                      (org-kit::export-prepare
                       (parse-org source) backend
                       :include-resolver
                       (lambda (path source-name)
                         (declare (ignore path source-name))
                         (incf include-calls)
                         "Included text.")))
                    (headline
                      (first (export-core-test-view-nodes
                              preparation :headline)))
                    (source-block
                      (first (export-core-test-view-nodes
                              preparation :src-block))))
               (with-soft-assertions
                 (expect headline :to-be-truthy)
                 (when headline
                   (expect (org-kit::export-view-children headline)
                           :to-be nil)
                   (expect (getf (org-kit::export-view-payload headline)
                                 :title-macros)
                           :to-be nil))
                 (expect (export-core-test-view-nodes preparation :macro)
                         :to-be-truthy)
                 (expect
                  (every (lambda (view)
                           (null (org-kit::export-view-children view)))
                         (export-core-test-view-nodes preparation :macro))
                  :to-be t)
                 (expect (export-core-test-view-nodes preparation :keyword)
                         :to-be-truthy)
                 (expect include-calls :to-be 0)
                 (expect source-block :to-be-truthy)
                 (when source-block
                   (expect (org-kit::export-view-children source-block)
                           :to-be nil)))))
        (when registered-p
          (unregister-export-backend backend)))))
  (it "skips headline title macros when child preparation is pruned"
    (let ((backend :api-export-pruned-headline-title)
          (registered-p nil)
          (preparation nil)
          (condition nil))
      (unwind-protect
           (progn
             (register-derived-export-backend
              backend :ascii
              :node-policies (list (cons :headline :prune-children)))
             (setf registered-p t)
             (setf condition
                   (export-core-test-catch-error
                    (lambda ()
                      (setf preparation
                            (org-kit::export-prepare
                             (parse-org "* {{{unknown-title-macro}}}")
                             backend)))))
             (let ((headline
                     (first (export-core-test-view-nodes
                             preparation :headline))))
               (with-soft-assertions
                 (expect condition :to-be nil)
                 (expect headline :to-be-truthy)
                 (when headline
                   (expect (org-kit::export-view-children headline)
                           :to-be nil)
                   (expect (getf (org-kit::export-view-payload headline)
                                 :title-macros)
                           :to-be nil)))))
        (when registered-p
          (unregister-export-backend backend)))))

  (it "defers unsupported special nodes to registry dispatch"
    (let ((renderer
            (lambda (context)
              (with-output-to-string (output)
                (dolist (view (export-render-context-root-views context))
                  (write-string (export-render-view context view) output))))))
      (labels ((export-with-policy (backend type source)
                 (let ((registered-p nil))
                   (unwind-protect
                        (progn
                          (register-export-backend
                           backend :renderer renderer
                           :node-policies
                           (list
                            (cons type
                                  (list :unsupported
                                        :feature :registry-policy-test
                                        :message "Unsupported by test policy"))))
                          (setf registered-p t)
                          (export-core-test-catch-error
                           (lambda () (export-org (parse-org source) backend))))
                     (when registered-p
                       (unregister-export-backend backend))))))
        (let ((keyword-condition
                (export-with-policy
                 :api-export-unsupported-keyword :keyword
                 "#+INCLUDE: \"child.org\""))
              (macro-condition
                (export-with-policy
                 :api-export-unsupported-macro :macro
                 "{{{unknown-macro}}}"))
              (source-condition
                (export-with-policy
                 :api-export-unsupported-source-block :src-block
                 (export-core-test-lines
                  "#+BEGIN_SRC text :exports unsupported"
                  "source text" "#+END_SRC"))))
          (with-soft-assertions
            (dolist (condition
                     (list keyword-condition macro-condition source-condition))
              (expect (typep condition 'org-export-unsupported-syntax)
                      :to-be t)
              (when (typep condition 'org-export-unsupported-syntax)
                (expect (org-export-unsupported-syntax-feature condition)
                        :to-be :registry-policy-test)
                (expect (org-export-unsupported-syntax-backend condition)
                        :to-be (cond
                                 ((eq condition keyword-condition)
                                  :api-export-unsupported-keyword)
                                 ((eq condition macro-condition)
                                  :api-export-unsupported-macro)
                                 (t :api-export-unsupported-source-block))))))))))

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

  (it "finds template text within parsed nodes and falls back when unavailable"
    (let* ((document (parse-org "prefix needle suffix"))
           (node
             (let ((paragraph nil))
               (map-nodes
                (lambda (candidate)
                  (when (eq (node-type candidate) :paragraph)
                    (setf paragraph candidate)))
                (document-root document))
               paragraph))
           (detached-node (make-node :paragraph :begin 17 :end 24)))
      (with-soft-assertions
        (expect node :to-be-truthy)
        (expect (org-kit::export-node-text-position node "needle") :to-be 7)
        (expect (org-kit::export-node-text-position node "needle" 3) :to-be 10)
        (expect (org-kit::export-node-text-position node "absent") :to-be 0)
        (expect (org-kit::export-node-text-position detached-node "missing")
                :to-be 17)
        (expect (org-kit::export-node-text-position nil "missing") :to-be 0))))

  (it "signals public export conditions with their backend, reason, and source"
    (let ((unknown-backend
            (export-core-test-catch-error
             (lambda ()
               (org-kit::export-prepare (parse-org "Body") :bogus))))
          (unsupported
            (export-core-test-catch-error
             (lambda ()
               (export-org (parse-org "%%(org-calendar-holiday)") :html)))))
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

  (it "registers node types and permits custom keywords referenced by static macros"
    (let* ((known-types (sort (copy-list (node-types)) #'string<
                              :key #'symbol-name))
           (transcoder-mapped-types
             (mapcar
              (lambda (backend)
                (let ((descriptor
                        (org-kit::export-registry-descriptor backend)))
                  (and descriptor
                       (sort
                        (mapcar
                         (function car)
                         (org-kit::%export-backend-descriptor-transcoders
                          descriptor))
                        (function string<) :key (function symbol-name)))))
              (list :html :gfm :latex)))
           (unknown-keyword-source
             (export-core-test-lines "Body" "" "#+UNSUPPORTED: value"))
           (prepared-unsupported
             (org-kit::export-prepare
              (parse-org
               (export-core-test-policy-fixture-source "unsupported-diary.org"))
              :html))
           (unsupported
             (export-core-test-catch-error
              (lambda ()
                (export-org
                 (parse-org
                  (export-core-test-policy-fixture-source "unsupported-diary.org"))
                 :html))))
           (unknown-keyword
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation unknown-keyword-source))))
           (macro-template-preparation
             (export-core-test-preparation
              (export-core-test-lines
               "#+MACRO: m {{{keyword(TOPIC)}}}"
               "#+TOPIC: value"
               "{{{m}}}")))
           (macro-template-texts
             (mapcar
              (lambda (view)
                (getf (org-kit::export-view-payload view) :expanded-text))
              (export-core-test-view-nodes macro-template-preparation :macro))))
      (with-soft-assertions
        (expect (every (lambda (types) (equal types known-types)) transcoder-mapped-types)
                :to-be t)
        (expect (typep prepared-unsupported 'org-kit::export-preparation)
                :to-be t)
        (expect (typep unsupported 'org-export-unsupported-syntax) :to-be t)
        (when (typep unsupported 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unsupported)
                  :to-be :diary-sexp))
        (expect (typep unknown-keyword 'org-export-unsupported-syntax) :to-be t)
        (when (typep unknown-keyword 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature unknown-keyword)
                  :to-be :unknown-keyword)
          (expect (org-export-unsupported-syntax-position unknown-keyword)
                  :to-be (search "#+UNSUPPORTED" unknown-keyword-source)))
        (progn
          (expect macro-template-texts
                  :to-equal '("{{{keyword(TOPIC)}}}" "value"))
          (expect
           (mapcar
            (lambda (view)
              (getf (org-kit::export-view-payload view) :expanded-text))
            (export-core-test-view-nodes
             (export-core-test-preparation
              (export-core-test-lines
               "#+MACRO: m {{{keyword($1)}}}"
               "#+TOPIC: value"
               "#+OTHER: second"
               "{{{m(TOPIC)}}}"
               "{{{m(OTHER)}}}"))
             :macro))
           :to-equal
           '("{{{keyword(TOPIC)}}}" "value"
             "{{{keyword(OTHER)}}}" "second"))))))

  (it "retains common citation processor keywords during preparation"
    (let* ((preparation
             (export-core-test-preparation
              (export-core-test-lines
               "#+CITE_EXPORT: basic" "#+BIBLIOGRAPHY: references.bib"
               "#+PRINT_BIBLIOGRAPHY: t" "" "Body")))
           (keys
             (loop for view in (export-core-test-all-views preparation)
                   for node = (org-kit::export-view-node view)
                   when (eq (node-type node) :keyword)
                     collect (node-property node :key))))
      (with-soft-assertions
        (expect keys :to-equal '("CITE_EXPORT" "BIBLIOGRAPHY"
                                 "PRINT_BIBLIOGRAPHY")))))

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
           (duplicate-language
             (export-core-test-preparation
              (export-core-test-lines "#+LANGUAGE: en" "#+LANGUAGE: ja")))
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
       (let ((duplicate-keyword
              (export-core-test-catch-error
               (lambda ()
                 (export-core-test-preparation
                  (export-core-test-lines
                   "#+SELECT_TAGS: first"
                   "#+SELECT_TAGS: second"))))))
         (expect (typep duplicate-keyword
                        'org-export-unsupported-syntax)
                 :to-be t)
         (when (typep duplicate-keyword 'org-export-unsupported-syntax)
           (expect (org-export-unsupported-syntax-feature duplicate-keyword)
                   :to-be :duplicate-keyword)))
       (expect (typep empty-macro-name 'org-export-unsupported-syntax)
               :to-be t)
       (expect (typep invalid-tags 'org-export-unsupported-syntax) :to-be t)
       (expect (every (lambda (condition)
                        (typep condition 'org-export-unsupported-syntax))
                      option-failures)
               :to-be t)
       (expect (getf (org-kit::export-preparation-metadata duplicate-language) :language) :to-equal "ja")
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
                 (export-core-test-lines
                  "#+MACRO: choose (eval (+ 1 2))"
                  "{{{choose(value)}}}")))))
           (invalid-macro-name
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation "#+MACRO: bad$ value"))))
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
           (multiple-digit-preparation
             (export-core-test-preparation
              (export-core-test-lines
               "#+MACRO: many [$3][$12][$13][$0][$000][$x]"
               "{{{many(one,two,three,four,five,six,seven,eight,nine,ten,eleven,twelve)}}}")))
           (multiple-digit-view
             (first (export-core-test-view-nodes multiple-digit-preparation :macro)))
           (multiple-digit-parameters
             (getf (org-kit::export-view-payload multiple-digit-view)
                   :expanded-text)))
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
        (expect (typep function-macro 'org-elisp-evaluator-error) :to-be t)
        (when (typep function-macro 'org-elisp-evaluator-error)
          (expect (org-callback-error-kind function-macro) :to-equal "EVALUATOR")
          (expect (org-callback-error-name function-macro) :to-equal "emacs-lisp")
          (expect (org-callback-error-reason function-macro) :to-be :unregistered))
        (expect (typep invalid-macro-name 'org-export-unsupported-syntax)
                :to-be t)
        (when (typep invalid-macro-name 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature invalid-macro-name)
                  :to-be :invalid-macro))
        (expect short-dollar :to-equal "$")
        (expect nondigit-parameter :to-equal "$x")
        (expect multiple-digit-parameters
                :to-equal "[three][twelve][][one][one][$x]"))))
  (it "passes function macro templates and parsed arguments to a caller evaluator"
    (let* ((context (make-export-context :title "Caller"))
           (registry (make-callback-registry))
           (calls nil)
           (source
             (export-core-test-lines
              "#+MACRO: echo (eval (format \"%s\" $1))"
              "{{{echo(first,second,)}}}"
              "{{{echo}}}"
              "{{{echo(nil)}}}"
              "{{{echo(number)}}}"
              "{{{echo(string)}}}"))
           (document (parse-org source)))
      (register-callback
       registry :evaluator :emacs-lisp
       (lambda (expression evaluator-context)
         (push (list expression evaluator-context) calls)
         (let ((arguments (getf evaluator-context :arguments)))
           (cond
             ((equal arguments '("first" "second" ""))
              (format nil "[~{~A~^|~}]" arguments))
             ((null arguments) "no-args")
             ((equal arguments '("nil")) nil)
             ((equal arguments '("number")) 27)
             ((equal arguments '("string")) "from evaluator")
             (t "unexpected")))))
      (let* ((preparation
               (org-kit::export-prepare
                document :html :context context :callback-registry registry))
             (views (export-core-test-view-nodes preparation :macro))
             (texts
               (mapcar (lambda (view)
                         (getf (org-kit::export-view-payload view) :expanded-text))
                       views))
             (source-arguments
               (node-property (org-kit::export-view-node (first views)) :args))
             (public-output
               (org-kit:export-org
                (parse-org
                 (export-core-test-lines
                  "#+MACRO: echo (eval (format \"%s\" $1))"
                  "{{{echo(string)}}}"))
                :ascii :context context :callback-registry registry))
             (records (nreverse calls))
             (arguments
               (mapcar (lambda (record)
                         (getf (second record) :arguments))
                       records))
             (expressions (mapcar #'first records)))
        (with-soft-assertions
          (expect texts
                  :to-equal '("[first|second|]" "no-args" "" "27" "from evaluator"))
          (expect (search "from evaluator" public-output) :to-be-truthy)
          (expect
           (typep
            (export-core-test-catch-error
             (lambda ()
               (org-kit::export-prepare
                document :html :callback-registry :invalid)))
            'type-error)
           :to-be t)
          (expect expressions
                  :to-equal
                  (make-list 6 :initial-element "(eval (format \"%s\" $1))"))
          (expect arguments
                  :to-equal
                  '(("first" "second" "") nil ("nil") ("number")
                    ("string") ("string")))
          (expect
           (every (lambda (record)
                    (equal (getf (second record) :macro-name) "echo"))
                  records)
           :to-be t)
          (expect
           (every (lambda (record)
                    (eq (getf (second record) :export-context) context))
                  records)
           :to-be t)
          (expect (eq (first arguments) source-arguments) :to-be nil)
          (expect (eq (first (first arguments)) (first source-arguments))
                  :to-be nil))
      (let* ((condition
              (make-condition 'simple-error
                              :format-control "function macro callback failure"))
             (throwing-registry (make-callback-registry))
             (caught nil))
        (register-callback
         throwing-registry :evaluator :emacs-lisp
         (lambda (expression evaluator-context)
           (declare (ignore expression evaluator-context))
           (error condition)))
        (setf caught
              (export-core-test-catch-error
               (lambda ()
                 (org-kit::export-prepare
                  (parse-org
                   (export-core-test-lines
                    "#+MACRO: fail (eval (+ 1 2))"
                    "{{{fail}}}"))
                  :html :callback-registry throwing-registry))))
        (expect (eq caught condition) :to-be t)
        (let* ((immediate-registry (make-callback-registry))
               (immediate-calls nil)
               (immediate-output
                (progn
                  (register-callback
                   immediate-registry :evaluator :emacs-lisp
                   (lambda (expression evaluator-context)
                     (push (list expression evaluator-context) immediate-calls)
                     "closed"))
                  (org-kit:export-org
                   (parse-org
                    (export-core-test-lines
                     "#+MACRO: immediate (eval)"
                     "{{{immediate}}}"))
                   :ascii :callback-registry immediate-registry)))
               (immediate-call (first immediate-calls)))
          (with-soft-assertions
           (expect (search "closed" immediate-output) :to-be-truthy)
           (expect (first immediate-call) :to-equal "(eval)")
           (expect (getf (second immediate-call) :macro-name)
                   :to-equal "immediate")))))))
  (it "keeps evaluator template mutations local to each call"
    (let* ((registry (make-callback-registry))
           (expressions nil)
           (document
             (parse-org
              (export-core-test-lines
               "#+MACRO: echo (eval (+ 1 2))"
               "{{{echo(first)}}}"
               "{{{echo(second)}}}"))))
      (register-callback
       registry :evaluator :emacs-lisp
       (lambda (expression context)
         (declare (ignore context))
         (push (copy-seq expression) expressions)
         (setf (char expression 0) #\x)
         "ok"))
      (let* ((preparation
               (org-kit::export-prepare
                document :html :callback-registry registry))
             (texts
               (mapcar (lambda (view)
                         (getf (org-kit::export-view-payload view) :expanded-text))
                       (export-core-test-view-nodes preparation :macro))))
        (with-soft-assertions
          (expect texts :to-equal (quote ("ok" "ok")))
          (expect (nreverse expressions)
                  :to-equal
                  (quote ("(eval (+ 1 2))" "(eval (+ 1 2))")))))))
  (it "copies string evaluator arguments and preserves other values"
    (let* ((registry (make-callback-registry))
           (argument (copy-seq "source"))
           (received nil)
           (result
             (progn
               (register-callback
                registry :evaluator :emacs-lisp
                (lambda (expression context)
                  (declare (ignore expression))
                  (setf received (getf context :arguments))
                  (setf (char (first received) 0) #\X)
                  "evaluated"))
               (org-kit::export-evaluate-function-macro
                (list :template "(eval)") "sample"
                (list argument :marker)
                (make-export-context)
                registry))))
      (with-soft-assertions
        (expect result :to-equal "evaluated")
        (expect argument :to-equal "source")
        (expect (first received) :to-equal "Xource")
        (expect (eq (first received) argument) :to-be nil)
        (expect (eq (second received) :marker) :to-be t))))
  (it "uses the name prefix in parenthesized macro definitions"
    (let* ((preparation
             (export-core-test-preparation
              (export-core-test-lines
               "#+MACRO: legacy(args) $1"
               "{{{legacy(value)}}}")))
           (views (export-core-test-view-nodes preparation :macro))
           (text (getf (org-kit::export-view-payload (first views))
                       :expanded-text)))
      (with-soft-assertions
        (expect (length views) :to-be 1)
        (expect text :to-equal "value"))))

  (it "rejects long numeric options and expands oversized macro indices to empty"
    (let* ((digits (make-string 4096 :initial-element #\9))
           (option-error
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 (concatenate 'string "#+OPTIONS: toc:" digits)))))
           (macro-preparation
             (export-core-test-preparation
              (export-core-test-lines
               (concatenate 'string "#+MACRO: many $" digits)
               "{{{many}}}")))
           (macro-view (first (export-core-test-view-nodes macro-preparation :macro)))
           (macro-text (getf (org-kit::export-view-payload macro-view)
                             :expanded-text)))
      (with-soft-assertions
        (expect (typep option-error 'org-export-unsupported-syntax) :to-be t)
        (when (typep option-error 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature option-error)
                  :to-be :invalid-option))
        (expect macro-text :to-equal ""))))
  (it "ignores unrelated children while reading property macro values"
    (let* ((document
             (parse-org
              (export-core-test-lines
               "* Heading"
               ":PROPERTIES:"
               ":VALUE: stored"
               ":END:"
               "{{{property(VALUE)}}}")))
           (drawer nil))
      (map-nodes
       (lambda (node)
         (when (eq (node-type node) :property-drawer)
           (setf drawer node)))
       (document-root document))
      (with-soft-assertions
        (expect drawer :not :to-be nil)
        (when drawer
          (setf (node-children drawer)
                (cons (make-node :paragraph) (node-children drawer)))
          (let* ((preparation (org-kit::export-prepare document :html))
                 (texts
                   (mapcar
                    (lambda (view)
                      (getf (org-kit::export-view-payload view) :expanded-text))
                    (export-core-test-view-nodes preparation :macro))))
            (expect texts :to-equal (list "stored")))))))
  (it "expands title, author, email, date, and keyword builtins"
    (let* ((source
             (export-core-test-lines
              "#+TITLE: Parsed $1 title"
              "#+TITLE: duplicate"
              "#+AUTHOR: Parsed $1 author"
              "#+AUTHOR: second author"
              "#+EMAIL: parsed+$1@example.test"
              "#+EMAIL: ignored@example.test"
              "#+DATE: $1 literal date"
              "#+DATE: second date"
              "#+TOPIC: first $1"
              "#+TOPIC: second"
              "{{{title(name)}}}"
              "{{{author(name)}}}"
              "{{{email(test)}}}"
              "{{{date(now)}}}"
              "{{{keyword(EMAIL)}}}"
              "{{{keyword(TOPIC)}}}"
              "{{{keyword(MISSING)}}}"))
           (preparation (export-core-test-preparation source))
           (metadata (org-kit::export-preparation-metadata preparation))
           (expanded
             (mapcar (lambda (view)
                       (getf (org-kit::export-view-payload view) :expanded-text))
                     (export-core-test-view-nodes preparation :macro)))
           (overridden
             (export-core-test-preparation
              (export-core-test-lines
               "#+TITLE: Parsed title"
               "#+AUTHOR: Parsed author"
               "#+AUTHOR: second author"
               "#+EMAIL: parsed@example.test"
               "#+DATE: 2025-05-04"
               "#+DATE: second date"
               "#+MACRO: title Local title $1"
               "#+MACRO: keyword Local keyword $1"
               "{{{title(context)}}}"
               "{{{author}}}"
               "{{{email}}}"
               "{{{date}}}"
               "{{{keyword(TITLE)}}}")
              :context (make-export-context
                        :title "Context title"
                        :author "Context author"
                        :date "Context date")))
           (overridden-metadata
             (org-kit::export-preparation-metadata overridden))
           (overridden-texts
             (mapcar
              (lambda (view)
                (getf (org-kit::export-view-payload view) :expanded-text))
              (export-core-test-view-nodes overridden :macro))))
      (with-soft-assertions
        (expect expanded
                :to-equal '("Parsed name title duplicate"
                            "Parsed name author second author"
                            "parsed+test@example.test"
                            "now literal date"
                            "parsed+$1@example.test ignored@example.test"
                            "first $1 second"
                            ""))
        (expect (getf metadata :date)
                :to-equal "$1 literal date second date")
        (expect overridden-texts
                :to-equal '("Local title context" "Context author"
                            "parsed@example.test" "2025-05-04"
                            "Local keyword TITLE"))
        (expect (getf overridden-metadata :date) :to-equal "Context date"))))
  (it "expands context-backed built-in macros"
  (let* ((time-context
           (make-time-context
            :reference-time (make-org-time-value "2030-01-02T03:04:05")
            :zone (make-fixed-time-zone 0)))
         (time-preparation
           (export-core-test-preparation
            "{{{time(%F %T %a|%^a %A|%^A %b|%^b %B|%^B %-d %_d %0d)}}}"
            :context (make-export-context :time-context time-context)))
         (time-texts
           (mapcar (lambda (view)
                     (getf (org-kit::export-view-payload view) :expanded-text))
                   (export-core-test-view-nodes time-preparation :macro)))
         (missing-time
           (export-core-test-catch-error
            (lambda ()
              (export-core-test-preparation "{{{time(%F)}}}"))))
         (missing-time-format
           (export-core-test-catch-error
            (lambda ()
              (export-core-test-preparation
               "{{{time}}}"
               :context (make-export-context :time-context time-context)))))
         (modification-preparation
           (export-core-test-preparation
            "{{{modification-time(%F %T)}}}"
            :context
            (make-export-context
             :extensions (list (cons :modification-time time-context)))))
         (modification-texts
           (mapcar (lambda (view)
                     (getf (org-kit::export-view-payload view) :expanded-text))
                   (export-core-test-view-nodes modification-preparation :macro)))
         (missing-modification-preparation (export-core-test-preparation "{{{modification-time}}}"))
         (missing-modification-format
           (export-core-test-catch-error
            (lambda ()
              (export-core-test-preparation
               "{{{modification-time}}}"
               :context
               (make-export-context
                :extensions (list (cons :modification-time time-context)))))))
         (input-file-preparation
           (org-kit::export-prepare
            (parse-org "{{{input-file}}}" :source-name "docs/guide.org")
            :html))
         (missing-input-file-preparation
           (export-core-test-preparation "{{{input-file}}}"))
         (property-preparation
           (export-core-test-preparation
            (export-core-test-lines
             "* Heading"
             ":PROPERTIES:"
             ":VALUE: first"
             ":CUSTOM_ID: custom"
             ":ID: id-value"
             ":END:"
             "{{{property(VALUE)}}}"
             "{{{property(MISSING)}}}"
             "{{{property(VALUE,Heading)}}}"
             "{{{property(VALUE,#custom)}}}")))
         (property-texts
           (mapcar (lambda (view)
                     (getf (org-kit::export-view-payload view) :expanded-text))
                   (export-core-test-view-nodes property-preparation :macro)))
         (missing-property-location
           (export-core-test-catch-error
            (lambda ()
              (export-core-test-preparation
               (export-core-test-lines
                "* Heading"
                "{{{property(VALUE,Absent)}}}")))))
         (counter-preparation
           (export-core-test-preparation
            (export-core-test-lines
             "{{{n(step)}}} {{{n(step)}}} {{{n(step,-)}}}"
             "{{{n(step,5)}}} {{{n(step)}}} {{{n(step,restart)}}}")))
         (counter-texts
           (mapcar (lambda (view)
                     (getf (org-kit::export-view-payload view) :expanded-text))
                   (export-core-test-view-nodes counter-preparation :macro)))
         (results-preparation
           (export-core-test-preparation
            (export-core-test-lines
             "#+RESULTS: ignored"
             "{{{results(value)}}}"
             "{{{results}}}")))
         (results-texts
           (mapcar (lambda (view)
                     (getf (org-kit::export-view-payload view) :expanded-text))
                   (export-core-test-view-nodes results-preparation :macro)))
         (overridden-preparation
           (export-core-test-preparation
            (export-core-test-lines
             "#+MACRO: n Local $1"
             "{{{n(step)}}}")))
         (overridden-texts
           (mapcar (lambda (view)
                     (getf (org-kit::export-view-payload view) :expanded-text))
                   (export-core-test-view-nodes overridden-preparation :macro))))
    (with-soft-assertions
      (progn
        (expect
         time-texts
         :to-equal
         '("2030-01-02 03:04:05 Wed|WED Wednesday|WEDNESDAY Jan|JAN January|JANUARY 2  2 02"))
        (expect
         (flet ((time-zone-macro-texts (offset)
                  (let ((preparation
                          (export-core-test-preparation
                           "{{{time(%z)}}}"
                           :context
                           (make-export-context
                            :time-context
                            (make-time-context
                             :reference-time
                             (make-org-time-value "2030-01-02T03:04:05")
                             :zone (make-fixed-time-zone offset))))))
                    (mapcar
                     (lambda (view)
                       (getf (org-kit::export-view-payload view) :expanded-text))
                     (export-core-test-view-nodes preparation :macro)))))
           (list (time-zone-macro-texts 0)
                 (time-zone-macro-texts 330)))
         :to-equal '(("+0000") ("+0530"))))
      (expect (typep missing-time 'org-export-unsupported-syntax) :to-be t)
      (when (typep missing-time 'org-export-unsupported-syntax)
        (expect (org-export-unsupported-syntax-feature missing-time)
                :to-be :macro-context-required))
      (progn
        (progn
          (expect modification-texts :to-equal '("2030-01-02 03:04:05"))
          (expect
           (flet ((modification-macro-texts (macro-text vc-context)
                    (let ((preparation
                            (export-core-test-preparation
                             macro-text
                             :context
                             (make-export-context
                              :extensions
                              (append
                               (list (cons :modification-time time-context))
                               (when vc-context
                                 (list (cons :modification-time-vc
                                             vc-context))))))))
                      (mapcar
                       (lambda (view)
                         (getf (org-kit::export-view-payload view) :expanded-text))
                       (export-core-test-view-nodes preparation :macro)))))
             (let ((vc-context
                     (make-time-context
                      :reference-time (make-org-time-value "2031-03-04T05:06:07")
                      :zone (make-fixed-time-zone 0))))
               (list
                (modification-macro-texts
                 "{{{modification-time(%F,vc)}}}" vc-context)
                (modification-macro-texts
                 "{{{modification-time(%F,vc)}}}" nil)
                (modification-macro-texts
                 "{{{modification-time(%F)}}}" vc-context)
                (modification-macro-texts
                 "{{{modification-time(%F,   )}}}" vc-context))))
           :to-equal '(("2031-03-04") ("2030-01-02") ("2030-01-02") ("2030-01-02"))))
        (expect (typep missing-time-format 'org-export-unsupported-syntax) :to-be t)
        (when (typep missing-time-format 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature missing-time-format)
                  :to-be :macro-format-required))
        (expect (typep missing-modification-format 'org-export-unsupported-syntax)
                :to-be t)
        (when (typep missing-modification-format 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-feature missing-modification-format)
                  :to-be :macro-format-required)))
      (expect
       (mapcar (lambda (view)
                 (getf (org-kit::export-view-payload view) :expanded-text))
               (export-core-test-view-nodes missing-modification-preparation :macro))
       :to-equal '(""))
      (expect
       (mapcar (lambda (view)
                 (getf (org-kit::export-view-payload view) :expanded-text))
               (export-core-test-view-nodes input-file-preparation :macro))
       :to-equal '("guide.org"))
      (expect
       (mapcar (lambda (view)
                 (getf (org-kit::export-view-payload view) :expanded-text))
               (export-core-test-view-nodes missing-input-file-preparation :macro))
       :to-equal '(""))
      (expect property-texts :to-equal '("first" "" "first" "first"))
      (expect
       (typep missing-property-location 'org-export-unsupported-syntax)
       :to-be t)
      (when (typep missing-property-location 'org-export-unsupported-syntax)
        (expect (org-export-unsupported-syntax-feature missing-property-location)
                :to-be :macro-property-location))
      (expect counter-texts :to-equal '("1" "2" "2" "5" "6" "1"))
      (expect results-texts :to-equal '("value" ""))
      (expect overridden-texts :to-equal '("Local step")))))

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
               (list "" "\"part.org" "part.org\"" "\"a\"b\""
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
  (it "applies INCLUDE line ranges, minimum heading levels, and fragment-local STARTUP"
    (let* ((calls nil)
           (fragment (export-core-test-lines
                      "skip" "* First" "** Child" "* Omitted" "tail"))
           (top-level
             (export-core-test-preparation
              "#+INCLUDE: part.org :lines \"2-4\""
              :include-resolver
              (lambda (path parent-source)
                (declare (ignore parent-source))
                (push path calls)
                fragment)))
           (top-headings (export-core-test-view-nodes top-level :headline))
           (nested
             (export-core-test-preparation
              (export-core-test-lines
               "* Parent"
               "#+INCLUDE: part.org :lines \"1-0\"")
              :include-resolver
              (lambda (path parent-source)
                (declare (ignore parent-source))
                (push path calls)
                fragment)))
           (nested-headings
             (remove-if
              (lambda (view)
                (string= (node-property (org-kit::export-view-node view)
                                        :raw-value)
                         "Parent"))
              (export-core-test-view-nodes nested :headline)))
           (explicit
             (export-core-test-preparation
              "#+INCLUDE: part.org :minlevel 3"
              :include-resolver
              (lambda (path parent-source)
                (declare (ignore parent-source))
                (push path calls)
                fragment)))
           (explicit-heading
             (first (export-core-test-view-nodes explicit :headline))))
      (with-soft-assertions
        (expect (mapcar (lambda (view)
                          (node-property (org-kit::export-view-node view)
                                         :raw-value))
                        top-headings)
                :to-equal '("First" "Child"))
        (expect (mapcar (lambda (view)
                          (node-property (org-kit::export-view-node view) :level))
                        top-headings)
                :to-equal '(1 2))
        (expect (mapcar (lambda (view)
                          (node-property (org-kit::export-view-node view) :level))
                        nested-headings)
                :to-equal '(2 3 2))
        (expect (node-property (org-kit::export-view-node explicit-heading) :level)
                :to-be 3)
        (expect (nreverse calls)
                :to-equal '("part.org" "part.org" "part.org")))))
  (it "wraps included text as source, example, and export blocks"
    (let* ((source-body (export-core-test-lines "* literal" "#+END_SRC"))
           (example-body (export-core-test-lines "* example" "#+TITLE: literal"))
           (export-body "#+END_EXPORT")
           (source-preparation
             (export-core-test-preparation
              "#+INCLUDE: part.org src lisp"
              :include-resolver
              (lambda (path parent-source)
                (declare (ignore path parent-source))
                source-body)))
           (example-preparation
             (export-core-test-preparation
              "#+INCLUDE: part.org example"
              :include-resolver
              (lambda (path parent-source)
                (declare (ignore path parent-source))
                example-body)))
           (export-preparation
             (export-core-test-preparation
              "#+INCLUDE: part.org export html"
              :include-resolver
              (lambda (path parent-source)
                (declare (ignore path parent-source))
                export-body)))
           (source-block
             (first (export-core-test-view-nodes source-preparation :src-block)))
           (example-block
             (first (export-core-test-view-nodes example-preparation :example-block)))
           (export-block
             (first (export-core-test-view-nodes export-preparation :export-block))))
      (with-soft-assertions
        (expect (node-property (org-kit::export-view-node source-block) :language)
                :to-equal "lisp")
        (expect (node-property (org-kit::export-view-node source-block) :value)
                :to-equal (format nil "* literal~%#+END_SRC~%"))
        (expect (export-core-test-view-nodes source-preparation :headline)
                :to-equal nil)
        (expect (node-property (org-kit::export-view-node example-block) :value)
                :to-equal (format nil "* example~%#+TITLE: literal~%"))
        (expect (node-property (org-kit::export-view-node export-block) :type)
                :to-equal "HTML")
        (expect (node-property (org-kit::export-view-node export-block) :value)
                :to-equal (format nil "#+END_EXPORT~%")))))
  (it "selects include headings, custom IDs, and named elements"
    (let* ((fragment
             (export-core-test-lines
              "* RangeRoot"
              "** First"
              "first"
              "** Second"
              "second"
              "** Third"
              "third"
              "* Custom"
              ":PROPERTIES:"
              ":CUSTOM_ID: selected-id"
              ":END:"
              "custom body"
              ""
              "#+NAME: named-note"
              "Named element body."))
           (calls nil)
           (preparation
             (export-core-test-preparation
              (export-core-test-lines
               "#+INCLUDE: part.org::*RangeRoot"
               "#+INCLUDE: part.org::*RangeRoot :lines \"2-6\""
               "#+INCLUDE: part.org::#selected-id"
               "#+INCLUDE: part.org::named-note")
              :include-resolver
              (lambda (path parent-source)
                (declare (ignore parent-source))
                (push path calls)
                fragment)))
           (headings (export-core-test-view-nodes preparation :headline))
           (named-element
             (find-if
              (lambda (view)
                (string= (or (node-property (org-kit::export-view-node view) :name)
                             "")
                         "named-note"))
              (export-core-test-view-nodes preparation :paragraph)))
           (missing-target
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 "#+INCLUDE: part.org::Missing"
                 :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore path parent-source))
                   fragment)))))
           (invalid-lines
             (export-core-test-catch-error
              (lambda ()
                (export-core-test-preparation
                 "#+INCLUDE: part.org :lines \"2\""
                 :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore path parent-source))
                   fragment))))))
      (with-soft-assertions
        (expect (mapcar (lambda (view)
                          (node-property (org-kit::export-view-node view) :raw-value))
                        headings)
                :to-equal '("RangeRoot" "First" "Second" "Third"
                            "First" "Second" "Third" "Custom"))
        (expect named-element :to-be-truthy)
        (expect (nreverse calls)
                :to-equal '("part.org" "part.org" "part.org" "part.org"))
        (expect (and (typep missing-target 'org-export-error)
                     (eq (org-export-error-reason missing-target)
                         :include-target-not-found))
                :to-be t)
        (expect (and (typep invalid-lines 'org-export-unsupported-syntax)
                     (eq (org-export-unsupported-syntax-feature invalid-lines)
                         :include-lines))
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
                      :adjacent-results)
                :to-be t)
        (expect (getf (org-kit::export-view-payload ignored-view) :exports)
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
                :to-equal '(:both :code :results :none :code :results))
        (expect (mapcar (lambda (payload) (getf payload :adjacent-results))
                        source-payloads)
                :to-equal '(t t t t nil nil))
        (expect result-child-counts :to-equal '(1 0 1 0 0 0))
        (expect separated-result :to-be-truthy)
        (expect separated-result-view :to-be-truthy)
        (expect (and separated-result
                     (string= (node-property separated-result :value)
                              "separated-result"))
                :to-be t))))

  (it "exports only the parts each source export mode selects in every backend"
    (let* ((document
             (parse-org
              (export-core-test-lines
               "#+BEGIN_SRC text :exports both" "both-code" "#+END_SRC"
               "#+RESULTS:" ": both-result"
               "#+BEGIN_SRC text :exports code" "code-code" "#+END_SRC"
               "#+RESULTS:" ": code-result"
               "#+BEGIN_SRC text" "default-code" "#+END_SRC"
               "#+RESULTS:" ": default-result [fn:secretlabel]"
               "#+BEGIN_SRC text :exports results" "results-code" "#+END_SRC"
               "#+RESULTS:" ": results-result"
               "#+BEGIN_SRC text :exports results" "unrun-code" "#+END_SRC"
               ""
               "#+BEGIN_SRC text :exports none" "none-code" "#+END_SRC"
               "#+RESULTS:" ": none-result")))
           (outputs (mapcar (lambda (backend) (export-org document backend))
                            '(:html :gfm :ascii))))
      (with-soft-assertions
        (dolist (output outputs)
          (expect (mapcar (lambda (text) (and (search text output) t))
                          '("both-code" "both-result" "code-code" "code-result"
                            "default-code" "default-result" "secretlabel"
                            "results-code" "results-result" "unrun-code"
                            "none-code" "none-result"))
                  :to-equal '(t t t nil t nil nil nil t nil nil nil))))))
  (it "exports sent radio tables and preserves lint findings"
    (labels ((find-table (node)
               (or (when (eq (node-type node) :table) node)
                   (some (function find-table) (node-children node))))
             (receive-body (text)
               (let* ((begin-marker "<!-- BEGIN RECEIVE ORGTBL radio -->")
                      (end-marker "<!-- END RECEIVE ORGTBL radio -->")
                      (begin (search begin-marker text))
                      (body-start (and begin
                                       (position #\Newline text :start begin)))
                      (end (and body-start
                                (search end-marker text :start2 body-start))))
                 (when (and body-start end)
                   (subseq text (1+ body-start) end)))))
      (let* ((source
               (export-core-test-lines
                "[[#missing]]"
                "#+ORGTBL: SEND radio orgtbl-to-orgtbl"
                "| Name | Value |"
                "|------+-------|"
                "| Ada  | one   |"
                "| Bob  | two   |"
                "<!-- BEGIN RECEIVE ORGTBL radio -->"
                "old"
                "<!-- END RECEIVE ORGTBL radio -->"))
             (document (parse-org source))
             (table (find-table (document-root document)))
             (edit (table-send-radio document table))
             (updated-document (edit-result-updated-document edit))
             (updated-source (document-text updated-document))
             (directive "#+ORGTBL: SEND radio orgtbl-to-orgtbl")
             (directive-start (search directive updated-source))
             (exportable-source
               (if directive-start
                   (concatenate 'string
                                (subseq updated-source 0 directive-start)
                                (subseq updated-source
                                        (+ directive-start (length directive))))
                   updated-source))
             (received-body (receive-body updated-source))
             (exportable-document (parse-org exportable-source))
             (before (org-lint exportable-document
                               :checkers (list "invalid-custom-id-link")))
             (output
               (let ((*export-baseline-current-test* nil))
                 (export-org exportable-document :html)))
             (after (org-lint exportable-document
                              :checkers (list "invalid-custom-id-link")))
             (first-table (search "<table" output))
             (second-table
               (and first-table
                    (search "<table" output
                            :start2 (+ first-table (length "<table")))))
             (third-table
               (and second-table
                    (search "<table" output
                            :start2 (+ second-table (length "<table")))))
             (first-ada
               (and first-table (search "Ada" output :start2 first-table)))
             (second-ada
               (and second-table (search "Ada" output :start2 second-table))))
        (with-soft-assertions
          (expect directive-start :to-be-truthy)
          (expect (search directive exportable-source) :to-be nil)
          (expect (search "[[#missing]]" exportable-source) :to-be-truthy)
          (expect received-body :to-be-truthy)
          (expect (receive-body exportable-source) :to-equal received-body)
          (expect (search "Ada" received-body) :to-be-truthy)
          (expect (length before) :to-be 1)
          (expect (length after) :to-be 1)
          (when (= 1 (length before))
            (expect (org-lint-diagnostic-checker (first before))
                    :to-equal "invalid-custom-id-link")
            (expect (org-lint-diagnostic-position (first before)) :to-be 1))
          (when (= 1 (length after))
            (expect (org-lint-diagnostic-checker (first after))
                    :to-equal "invalid-custom-id-link")
            (expect (org-lint-diagnostic-position (first after)) :to-be 1))
          (when (and (= 1 (length before)) (= 1 (length after)))
            (expect (org-lint-diagnostic-message (first before))
                    :to-equal (org-lint-diagnostic-message (first after)))
            (expect (search "missing"
                            (org-lint-diagnostic-message (first before)))
                    :to-be-truthy))
          (expect first-table :to-be-truthy)
          (expect second-table :to-be-truthy)
          (expect third-table :to-be nil)
          (expect first-ada :to-be-truthy)
          (expect second-ada :to-be-truthy)))))

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

  (it "accepts only restricted relative file paths"
    (with-soft-assertions
      (expect (org-kit::export-safe-relative-file-path-p "figure_1.png")
              :to-be t)
      (expect (org-kit::export-safe-relative-file-path-p "assets/figure.png")
              :to-be t)
      (expect (org-kit::export-safe-relative-file-path-p nil) :to-be nil)
      (expect (org-kit::export-safe-relative-file-path-p "") :to-be nil)
      (expect (org-kit::export-safe-relative-file-path-p "/etc/passwd.png")
              :to-be nil)
      (expect (org-kit::export-safe-relative-file-path-p "../secret.png")
              :to-be nil)
      (expect (org-kit::export-safe-relative-file-path-p "assets/../secret.png")
              :to-be nil)
      (expect (org-kit::export-safe-relative-file-path-p "bad path.png")
              :to-be nil)
      (expect (org-kit::export-safe-relative-file-path-p "bad\\path.png")
              :to-be nil)
      (expect (org-kit::export-safe-relative-file-path-p
               (format nil "line~Cbreak.png" (code-char 10)))
              :to-be nil)))

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
        (expect
         (and (typep invalid-raw-html 'type-error)
              (typep
               (export-core-test-catch-error
                (lambda ()
                  (org-kit::export-prepare
                   document :html :allow-raw-latex :invalid)))
               'type-error))
         :to-be t)
        (expect (typep unparsed 'type-error) :to-be t))))
  (it "formats the full time directive family and boundary cases"
      (let* ((context
              (make-export-context :time-context
                                   (make-time-context :reference-time
                                                      (make-org-time-value
                                                       "2030-01-02T03:04:05")
                                                      :zone
                                                      (make-fixed-time-zone 0))))
             (preparation
              (export-core-test-preparation
               "{{{time(%% %y %C %e %k %I %l %j %p %P %w %u %V %G %g %U %W %a %A %b %h %B %Z %s %D %R %r %c %x %X %n %t)}}}"
               :context
               context))
             (text
              (getf
               (org-kit::export-view-payload
                (first (export-core-test-view-nodes preparation :macro)))
               :expanded-text))
             (week-boundary
              (export-core-test-preparation "{{{time(%U %W)}}}"
                                            :context
                                            (make-export-context :time-context
                                                                 (make-time-context
                                                                  :reference-time
                                                                  (make-org-time-value
                                                                   "2030-01-13T03:04:05")
                                                                  :zone
                                                                  (make-fixed-time-zone
                                                                   0)))))
             (week-text
              (getf
               (org-kit::export-view-payload
                (first (export-core-test-view-nodes week-boundary :macro)))
               :expanded-text))
             (hour-texts
              (mapcar
               (lambda (timestamp)
                 (let* ((preparation
                         (export-core-test-preparation "{{{time(%I %l %p %P)}}}"
                                                       :context
                                                       (make-export-context
                                                        :time-context
                                                        (make-time-context
                                                         :reference-time
                                                         (make-org-time-value
                                                          timestamp)
                                                         :zone
                                                         (make-fixed-time-zone 0)))))
                        (view
                         (first (export-core-test-view-nodes preparation :macro))))
                   (getf (org-kit::export-view-payload view) :expanded-text)))
               '("2030-01-02T00:04:05" "2030-01-02T12:04:05")))
             (negative-zone
              (export-core-test-preparation "{{{time(%z)}}}"
                                            :context
                                            (make-export-context :time-context
                                                                 (make-time-context
                                                                  :reference-time
                                                                  (make-org-time-value
                                                                   "2030-01-02T03:04:05")
                                                                  :zone
                                                                  (make-fixed-time-zone
                                                                   -240)))))
             (negative-zone-text
              (getf
               (org-kit::export-view-payload
                (first (export-core-test-view-nodes negative-zone :macro)))
               :expanded-text))
             (trailing-texts
              (mapcar
               (lambda (view)
                 (getf (org-kit::export-view-payload view) :expanded-text))
               (export-core-test-view-nodes
                (export-core-test-preparation "{{{time(%)}}} {{{time(%-)}}}"
                                              :context
                                              context)
                :macro)))
             (unsupported (getf (org-kit::export-view-payload (first (export-core-test-view-nodes (export-core-test-preparation "{{{time(%Q)}}}" :context context) :macro))) :expanded-text)))
        (with-soft-assertions
         (expect text
                 :to-equal
                 (concatenate 'string
                              "% 30 20  2  3 03  3 002 AM am 3 3 01 2030 30 00 00 "
                              "Wed Wednesday Jan Jan January UTC 1893553445 "
                              "01/02/30 03:04 03:04:05 AM Wed Jan  2 03:04:05 2030 "
                              "01/02/30 03:04:05 "
                              (string #\Newline)
                              " "
                              (string #\Tab)))
         (expect week-text :to-equal "02 01")
         (expect hour-texts :to-equal '("12 12 AM am" "12 12 PM pm"))
         (expect negative-zone-text :to-equal "-0400")
         (expect trailing-texts :to-equal '("%" "%-"))
         (expect unsupported :to-equal "%Q"))))
  (it "validates include options and preserves target, range, and wrapper semantics"
      (let* ((invalid-options
              (mapcar
               (lambda (source)
                 (export-core-test-catch-error
                  (lambda ()
                    (export-core-test-preparation
                     source
                     :include-resolver
                     (lambda (path parent-source)
                       (declare (ignore path parent-source))
                       "Body")))))
               (list
                "#+INCLUDE: part.org :lines"
                "#+INCLUDE: part.org :lines 1-2"
                "#+INCLUDE: part.org :lines \"1-2\" :lines \"2-3\""
                "#+INCLUDE: part.org :lines \"1-2-3\""
                "#+INCLUDE: part.org :lines \"a-2\""
                "#+INCLUDE: part.org :lines \"-\""
                "#+INCLUDE: part.org :minlevel"
                "#+INCLUDE: part.org :minlevel -1"
                "#+INCLUDE: part.org :minlevel 2 :minlevel 3"
                "#+INCLUDE: part.org :only-contents"
                "#+INCLUDE: part.org :only-contents maybe"
                "#+INCLUDE: part.org :only-contents t :only-contents nil"
                "#+INCLUDE: part.org unsupported"
                "#+INCLUDE: part.org example extra")))
             (contents
              (export-core-test-preparation
               "#+INCLUDE: \"part.org::*Target\" :only-contents t"
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (export-core-test-lines "* Target" "Body text"))))
             (contents-nil
              (export-core-test-preparation
               "#+INCLUDE: \"part.org::*Target\" :only-contents nil"
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (export-core-test-lines "* Target" "Body text"))))
             (normalized-target
              (export-core-test-preparation
               "#+INCLUDE: \"part.org::*Root  Target\""
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (export-core-test-lines "* Root Target" "Selected body"))))
             (empty-target
              (export-core-test-preparation
               "#+INCLUDE: part.org::"
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (export-core-test-lines "* Whole fragment" "Whole body"))))
             (empty-target-headings (export-core-test-view-nodes empty-target :headline))
             (range-start-zero
              (export-core-test-preparation
               "#+INCLUDE: part.org :lines \"0-2\""
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (export-core-test-lines "First" "Second" "Third"))))
             (range-open-start
              (export-core-test-preparation
               "#+INCLUDE: part.org :lines \"-2\""
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (export-core-test-lines "First" "Second" "Third"))))
             (range-open-end
              (export-core-test-preparation
               "#+INCLUDE: part.org :lines \"2-\""
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (export-core-test-lines "First" "Second" "Third"))))
             (range-end
              (export-core-test-preparation
               "#+INCLUDE: part.org :lines \"8-9\""
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 "short")))
             (leading-blank-line
              (export-core-test-preparation
               "#+INCLUDE: part.org"
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (export-core-test-lines "   " "Body"))))
             (whitespace-only
              (export-core-test-preparation
               "#+INCLUDE: part.org"
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 "   ")))
             (source-wrapper
              (export-core-test-preparation
               "#+INCLUDE: part.org src lisp"
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (export-core-test-lines
                  "first"
                  "   "
                  ",,,,"))))
             (source-value
              (node-property
               (org-kit::export-view-node
                (first (export-core-test-view-nodes source-wrapper :src-block)))
               :value))
             (empty-source-wrapper
              (export-core-test-preparation
               "#+INCLUDE: part.org src lisp"
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 "")))
             (empty-source-value
              (node-property
               (org-kit::export-view-node
                (first
                 (export-core-test-view-nodes empty-source-wrapper :src-block)))
               :value))
             (newline-source-wrapper
              (export-core-test-preparation
               "#+INCLUDE: part.org src lisp"
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (format nil "Body~%"))))
             (newline-source-value
              (node-property
               (org-kit::export-view-node
                (first
                 (export-core-test-view-nodes newline-source-wrapper :src-block)))
               :value))
             (paragraph-texts
              (lambda (preparation)
                (mapcar
                 (lambda (view)
                   (string-trim '(#\Space #\Newline #\Return)
                                (node-text (org-kit::export-view-node view))))
                 (export-core-test-view-nodes preparation :paragraph))))
             (odd-levels
              (export-core-test-preparation
               (export-core-test-lines "#+INCLUDE: part.org :minlevel 3")
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore path parent-source))
                 (export-core-test-lines
                  "#+STARTUP: odd"
                  "* Odd root"
                  "*** Odd child"))))
             (odd-levels-stars
              (mapcar
               (lambda (view)
                 (node-property (org-kit::export-view-node view) :level))
               (export-core-test-view-nodes odd-levels :headline))))
        (with-soft-assertions
         (expect (every (lambda (condition)
                          (typep condition 'org-export-unsupported-syntax))
                        invalid-options)
                 :to-be t)
         (expect (export-core-test-view-nodes contents :headline) :to-equal nil)
         (expect (export-core-test-view-nodes contents-nil :headline)
                 :to-be-truthy)
         (expect
          (mapcar
           (lambda (view)
             (node-property (org-kit::export-view-node view) :raw-value))
           (export-core-test-view-nodes normalized-target :headline))
          :to-equal '("Root Target"))
         (expect
          (mapcar
           (lambda (view)
             (node-property (org-kit::export-view-node view) :raw-value))
           empty-target-headings)
          :to-equal '("Whole fragment"))
         (expect (funcall paragraph-texts range-start-zero) :to-equal '("First"))
         (expect (funcall paragraph-texts range-open-start) :to-equal '("First"))
         (expect (funcall paragraph-texts range-open-end)
                 :to-equal (list (format nil "Second~%Third")))
         (expect (export-core-test-view-nodes range-end :paragraph) :to-equal nil)
         (expect (funcall paragraph-texts leading-blank-line) :to-equal '("Body"))
         (expect (export-core-test-view-nodes whitespace-only :paragraph) :to-equal nil)
         (expect source-value :to-equal (format nil "first~%   ~%,,,,~%"))
         (expect empty-source-value :to-equal "")
         (expect newline-source-value :to-equal (format nil "Body~%"))
         (expect odd-levels-stars :to-equal (list 3 4)))))
  (it "filters archived setup files and caches static macro keyword templates"
      (let* ((resolver-calls nil)
             (preparation
              (export-core-test-preparation
               (export-core-test-lines
                "#+OPTIONS: arch:headline tasks:nil"
                "#+TODO: TODO | DONE"
                "#+SETUPFILE: root.setup"
                "* Archived :ARCHIVE:"
                "#+SETUPFILE: archived.setup"
                "* TODO Hidden"
                "#+SETUPFILE: todo.setup"
                "* DONE Completed"
                "#+SETUPFILE: done.setup"
                "* Active"
                "#+SETUPFILE: active.setup")
               :include-resolver
               (lambda (path parent-source)
                 (declare (ignore parent-source))
                 (push path resolver-calls)
                 "")))
             (blank-document (parse-org "#+SETUPFILE:"))
             (blank-records
              (nth-value
               0
               (org-kit::export-setupfile-keyword-records
                blank-document :html nil (document-limits blank-document)
                (org-kit::export-node-count (document-root blank-document))
                0)))
             (empty-path
              (export-core-test-catch-error
               (lambda ()
                 (export-core-test-preparation "#+SETUPFILE: \"\""))))
             (control-path
              (export-core-test-catch-error
               (lambda ()
                 (export-core-test-preparation
                  (format nil "#+SETUPFILE: \"bad~Cpath\"" #\Tab)))))
             (macro-document
              (parse-org
               (export-core-test-lines
                "#+MACRO: m {{{keyword(TOPIC)}}}"
                "#+MACRO: no-key {{{keyword}}}"
                "#+MACRO: empty-key {{{keyword(,TOPIC)}}}"
                "#+TOPIC: value"
                "{{{m}}}"
                "{{{no-key}}}"
                "{{{empty-key}}}")))
             (macro-records (org-kit::export-keyword-records macro-document))
             (macros
              (nth-value
               5
               (org-kit::export-document-settings
                macro-document :html nil nil macro-records)))
             (macro-cache (make-hash-table :test #'eq))
             (references
              (org-kit::export-macro-keyword-reference-set
               macro-document macros macro-cache))
             (template-cache (gethash macros macro-cache))
             (cached-template (gethash "m" template-cache))
             (large-template
              (format nil "#+MACRO: m ~{~A~^ ~}~%{{{m}}}"
                      (make-list 8 :initial-element "{{{keyword(TOPIC)}}}")))
             (limited-document
              (parse-org large-template :limits (make-limits :max-node-count 5)))
             (macro-limit-error
              (export-core-test-catch-error
               (lambda () (org-kit::export-prepare limited-document :html)))))
        (with-soft-assertions
         (expect (org-kit::export-preparation-p preparation) :to-be t)
         (expect (nreverse resolver-calls)
                 :to-equal '("root.setup" "active.setup"))
         (expect (mapcar #'second blank-records) :to-equal '("SETUPFILE"))
         (expect (typep empty-path 'org-export-unsupported-syntax) :to-be t)
         (when (typep empty-path 'org-export-unsupported-syntax)
           (expect (org-export-unsupported-syntax-feature empty-path)
                   :to-be :setupfile-form))
         (expect (typep control-path 'org-export-unsupported-syntax) :to-be t)
         (when (typep control-path 'org-export-unsupported-syntax)
           (expect (org-export-unsupported-syntax-feature control-path)
                   :to-be :setupfile-form))
         (expect (hash-table-p references) :to-be t)
         (expect (gethash "TOPIC" references) :to-be-truthy)
         (expect (nth-value 1 (gethash "" references)) :to-be nil)
         (expect cached-template :to-be-truthy)
         (expect (typep macro-limit-error 'org-resource-limit-error) :to-be t)
         (when (typep macro-limit-error 'org-resource-limit-error)
           (expect (org-resource-limit-error-limit-name macro-limit-error)
                   :to-be :max-node-count)
           (expect (org-resource-limit-error-limit macro-limit-error) :to-be 5)
           (expect (> (org-resource-limit-error-actual macro-limit-error) 5)
                   :to-be t)))))
  (it "splits paragraphs around whitespace-only macro expansions"
      (let* ((preparation
              (org-kit::export-prepare
               (parse-org
                (export-core-test-lines
                 "#+MACRO: blank"
                 "before"
                 "  {{{blank}}}  "
                 "after"
                 "  {{{blank}}}  "))
               :ascii))
             (paragraphs (export-core-test-view-nodes preparation :paragraph))
             (expanded-macros
              (mapcar
               (lambda (view)
                 (getf (org-kit::export-view-payload view) :expanded-text))
               (export-core-test-view-nodes preparation :macro)))
             (callback-registry (make-callback-registry))
             (runtime-preparation
              (progn
                (register-callback
                 callback-registry :evaluator :emacs-lisp
                 (lambda (expression context)
                   (declare (ignore expression))
                   (if (string= (getf context :macro-name) "spaces")
                       " "
                       "retained")))
                (org-kit::export-prepare
                 (parse-org
                  (export-core-test-lines
                   "#+MACRO: spaces (eval)"
                   "#+MACRO: visible (eval)"
                   "before"
                   "  {{{spaces}}}  "
                   "middle"
                   "  {{{visible}}}  "
                   "after"))
                 :ascii :callback-registry callback-registry)))
             (runtime-paragraphs
              (export-core-test-view-nodes runtime-preparation :paragraph))
             (runtime-macros
              (mapcar
               (lambda (view)
                 (getf (org-kit::export-view-payload view) :expanded-text))
               (export-core-test-view-nodes runtime-preparation :macro)))
             (boundary-preparation
              (org-kit::export-prepare
               (parse-org
                (export-core-test-lines
                 "#+MACRO: blank"
                 "{{{blank}}} "
                 "middle"
                 " {{{blank}}}"))
               :ascii))
             (suffix-preparation
              (org-kit::export-prepare
               (parse-org
                (export-core-test-lines
                 "#+MACRO: blank"
                 "before"
                 "  {{{blank}}}right"
                 "after"))
               :ascii))
             (input-limit-document
              (parse-org "#+INCLUDE: part.org"
                         :limits (make-limits :max-input-length 40)))
             (input-limit-error
              (export-core-test-catch-error
               (lambda ()
                 (org-kit::export-prepare
                  input-limit-document :html
                  :include-resolver
                  (lambda (path parent-source)
                    (declare (ignore path parent-source))
                    (make-string 80 :initial-element (char "a" 0))))))))
        (with-soft-assertions
         (expect (length paragraphs) :to-be 2)
         (expect expanded-macros :to-equal nil)
         (expect (length runtime-paragraphs) :to-be 3)
         (expect runtime-macros :to-equal '("retained"))
         (expect
          (length
           (export-core-test-view-nodes boundary-preparation :paragraph))
          :to-be 1)
         (expect
          (export-core-test-view-nodes boundary-preparation :macro)
          :to-equal nil)
         (expect
          (length
           (export-core-test-view-nodes suffix-preparation :paragraph))
          :to-be 1)
         (expect
          (mapcar
           (lambda (view)
             (getf (org-kit::export-view-payload view) :expanded-text))
           (export-core-test-view-nodes suffix-preparation :macro))
          :to-equal (list ""))
         (expect
          (length
           (export-core-test-view-nodes
            (org-kit::export-prepare
             (parse-org
              (export-core-test-lines
               "#+MACRO: blank"
               "left{{{blank}}} "
               " {{{blank}}}right"))
             :ascii)
            :paragraph))
          :to-be 1)
         (expect (typep input-limit-error 'org-resource-limit-error) :to-be t)
         (when (typep input-limit-error 'org-resource-limit-error)
           (expect (org-resource-limit-error-limit-name input-limit-error)
                   :to-be :max-input-length)))))
  (it "drops an empty built-in macro at source offset zero"
      (let* ((document (parse-org "{{{keyword(MISSING)}}}"))
             (preparation (org-kit::export-prepare document :ascii)))
        (with-soft-assertions
          (expect (export-core-test-view-nodes preparation :macro)
                  :to-equal nil)
          (expect (export-core-test-view-nodes preparation :paragraph)
                  :to-equal nil)))))

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
  (it "guards programmatic macro nodes with missing or out-of-range source spans"
    (let* ((base-source (format nil "#+MACRO: blank~%{{{blank}}}"))
           (suffix-source
             (format nil "#+MACRO: blank~% {{{blank}}}right"))
           (macro-length (length "{{{blank}}}")))
      (labels ((preparation (source begin end)
                 (let* ((document (make-document source))
                        (line-end (position #\Newline source))
                        (paragraph-begin (1+ line-end))
                        (macro-node
                          (make-node :macro :begin begin :end end
                                     :properties (list :key "blank" :args nil)))
                        (keyword-node
                          (make-node :keyword :begin 0 :end line-end
                                     :properties
                                     (list :key "MACRO" :value "blank")))
                        (paragraph-node
                          (make-node :paragraph :begin paragraph-begin
                                     :end (length source)
                                     :children (list macro-node)))
                        (root
                          (make-node :org-data :begin 0 :end (length source)
                                     :children (list keyword-node paragraph-node))))
                   (setf (document-root document) root)
                   (org-kit::export-prepare document :ascii))))
        (dolist (span
                 (list (list base-source nil nil)
                       (list base-source 0 nil)
                       (list base-source (1+ (length base-source))
                             (+ 2 (length base-source)))
                       (let ((start (search "{{{blank}}}" suffix-source)))
                         (list suffix-source start (+ start macro-length)))))
          (let* ((preparation (apply #'preparation span))
                 (paragraphs
                   (export-core-test-view-nodes preparation :paragraph))
                 (macros (export-core-test-view-nodes preparation :macro)))
            (expect (length paragraphs) :to-be 1)
            (expect (length macros) :to-be 1)
            (expect (getf (org-kit::export-view-payload (first macros))
                          :expanded-text)
                    :to-equal ""))))))
  (it "preserves parser resource errors while scanning and expanding macros"
    (let* ((static-template "template text")
           (static-document
             (parse-org
              (export-core-test-lines
               (concatenate 'string "#+MACRO: depth " static-template)
               "{{{depth}}}")))
           (static-records (org-kit::export-keyword-records static-document))
           (static-macros
             (nth-value
              5
              (org-kit::export-document-settings
               static-document :html nil nil static-records)))
           (original-parse-org (symbol-function 'parse-org))
           (static-parser-calls 0)
           (static-condition
             (unwind-protect
                  (progn
                    (setf (symbol-function 'parse-org)
                          (lambda (input &rest arguments)
                            (if (string= input static-template)
                                (progn
                                  (incf static-parser-calls)
                                  (org-kit::check-limit
                                   (make-limits :max-element-depth 3)
                                   :max-element-depth 4))
                                (apply original-parse-org input arguments))))
                    (export-core-test-catch-error
                     (lambda ()
                       (org-kit::export-macro-keyword-reference-set
                        static-document static-macros
                        (make-hash-table :test #'eq)))))
               (setf (symbol-function 'parse-org) original-parse-org)))
           (expanded-template "[[https://example.invalid][*bold /italic/*]]")
           (callback-registry (make-callback-registry))
           (expanded-document
             (parse-org
              (export-core-test-lines
               "#+MACRO: depth (eval)"
               "{{{depth}}}")))
           (expanded-parser-calls 0)
           (expanded-condition
             (unwind-protect
                  (progn
                    (register-callback
                     callback-registry :evaluator :emacs-lisp
                     (lambda (expression context)
                       (declare (ignore expression context))
                       expanded-template))
                    (setf (symbol-function 'parse-org)
                          (lambda (input &rest arguments)
                            (if (string= input expanded-template)
                                (progn
                                  (incf expanded-parser-calls)
                                  (org-kit::check-limit
                                   (make-limits :max-element-depth 2)
                                   :max-element-depth 3))
                                (apply original-parse-org input arguments))))
                    (export-core-test-catch-error
                     (lambda ()
                       (org-kit::export-prepare
                        expanded-document :html
                        :callback-registry callback-registry))))
               (setf (symbol-function 'parse-org) original-parse-org))))
      (with-soft-assertions
        (expect static-parser-calls :to-be 1)
        (expect (typep static-condition 'org-resource-limit-error) :to-be t)
        (when (typep static-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name static-condition)
                  :to-be :max-element-depth)
          (expect (org-resource-limit-error-limit static-condition)
                  :to-be 3)
          (expect (org-resource-limit-error-actual static-condition)
                  :to-be 4))
        (expect expanded-parser-calls :to-be 1)
        (expect (typep expanded-condition 'org-resource-limit-error) :to-be t)
        (when (typep expanded-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name expanded-condition)
                  :to-be :max-element-depth)
          (expect (org-resource-limit-error-limit expanded-condition)
                  :to-be 2)
          (expect (org-resource-limit-error-actual expanded-condition)
                  :to-be 3)))))

  (it "preserves parser limits when parsing expanded fragments"
    (let* ((node-limit-document
             (parse-org "#+INCLUDE: \"part.org\""
                        :limits (make-limits :max-node-count 4)))
           (node-limit-condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 node-limit-document :html
                 :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore path parent-source))
                   (export-core-test-lines
                    "* One" "* Two" "* Three" "* Four" "* Five"))))))
           (static-template-document
             (parse-org
              (export-core-test-lines
               "#+MACRO: nested [[https://example.invalid][*bold /italic/*]]"
               "{{{nested}}}")
              :limits (make-limits :max-element-depth 3)))
           (static-template-condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare static-template-document :html))))
           (include-depth-document
             (parse-org "#+INCLUDE: \"part.org\""
                        :limits (make-limits :max-element-depth 2)))
           (include-depth-condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 include-depth-document :html
                 :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore path parent-source))
                   (export-core-test-lines "* Heading" "Body"))))))
           (remap-source "#+INCLUDE: \"part.org\"")
           (remap-unbounded-document (parse-org remap-source))
           (remap-limit
             (1+ (org-kit::export-node-count
                  (document-root remap-unbounded-document))))
           (remap-document
             (parse-org remap-source
                        :limits
                        (make-limits :max-export-expanded-nodes remap-limit)))
           (remap-condition
             (export-core-test-catch-error
              (lambda ()
                (org-kit::export-prepare
                 remap-document :html
                 :include-resolver
                 (lambda (path parent-source)
                   (declare (ignore path parent-source))
                   (export-core-test-lines "* One" "Body")))))))
      (with-soft-assertions
        (expect (typep node-limit-condition 'org-resource-limit-error)
                :to-be t)
        (when (typep node-limit-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name node-limit-condition)
                  :to-be :max-node-count)
          (expect (org-resource-limit-error-limit node-limit-condition)
                  :to-be 4)
          (expect (> (org-resource-limit-error-actual node-limit-condition) 4)
                  :to-be t))
        (expect (typep static-template-condition 'org-resource-limit-error)
                :to-be t)
        (when (typep static-template-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name static-template-condition)
                  :to-be :max-element-depth)
          (expect (org-resource-limit-error-limit static-template-condition)
                  :to-be 3)
          (expect (> (org-resource-limit-error-actual static-template-condition) 3)
                  :to-be t))
        (expect (typep include-depth-condition 'org-resource-limit-error)
                :to-be t)
        (when (typep include-depth-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name include-depth-condition)
                  :to-be :max-element-depth)
          (expect (org-resource-limit-error-limit include-depth-condition)
                  :to-be 2)
          (expect (> (org-resource-limit-error-actual include-depth-condition) 2)
                  :to-be t))
        (expect (typep remap-condition 'org-resource-limit-error) :to-be t)
        (when (typep remap-condition 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name remap-condition)
                  :to-be :max-export-expanded-nodes)
          (expect (org-resource-limit-error-limit remap-condition)
                  :to-be remap-limit)
          (expect (> (org-resource-limit-error-actual remap-condition)
                     remap-limit)
                  :to-be t)))))

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
    (let* ((source
             (export-core-test-lines "#+MACRO: m $1" "{{{m(abc)}}}"))
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
                  :to-be :unsupported-backend)))))
  (it "validates filtered views, accepts TITLE, and checks export arguments, limits, and stages"
      (let ((backend :api-export-core-filter-validation)
            (mode :valid)
            (acyclic-result nil)
            (registered-p nil))
        (labels ((new-view (node &key children)
                           (make-export-view node :children children))
                 (run-export (&optional (limits (make-limits)))
                             (export-org (parse-org "Body" :limits limits) backend))
                 (parse-tree-filter (views context)
                                    (declare (ignore context))
                                    (let ((node (export-view-node (first views))))
                                      (case mode
                                        (:valid
                                         (let ((view (new-view node)))
                                           (setf acyclic-result
                                                 (typep view 'org-kit::acyclic-export-view))
                                           (list view)))
                                        (:invalid-root :not-a-list)
                                        (:invalid-view (list :not-an-export-view))
                                        (:invalid-node (list (org-kit::%make-export-view :node nil)))
                                        (:invalid-children
                                         (let ((view (org-kit::%make-export-view :node node
                                                                                 :children (cons nil :tail))))
                                           (setf acyclic-result
                                                 (typep view 'org-kit::acyclic-export-view))
                                           (list view)))
                                        (:invalid-child
                                         (let ((view (org-kit::%make-export-view :node node
                                                                                 :children (list :not-a-view))))
                                           (setf acyclic-result
                                                 (typep view 'org-kit::acyclic-export-view))
                                           (list view)))
                                        (:invalid-source
                                         (list (org-kit::%make-export-view
                                                :node node :source-document :not-a-document)))
                                        (:cycle
                                         (let* ((children (list nil))
                                                (view (org-kit::%make-export-view :node node
                                                                                  :children children)))
                                           (setf (car children) view
                                                 acyclic-result
                                                 (typep view 'org-kit::acyclic-export-view))
                                           (list view)))
                                        (:deep
                                         (let ((view (new-view node)))
                                           (loop repeat 8
                                                 do (setf view (new-view node :children (list view))))
                                           (list view)))
                                        (:too-many
                                         (loop repeat 20 append views))
                                        (otherwise
                                         (error "Unknown export-core coverage mode: ~S" mode))))))
          (register-export-backend
           backend
           :renderer (lambda (context)
                       (declare (ignore context))
                       "rendered")
           :node-policies (list (cons :keyword
                                      (list :unsupported :feature :test-keyword-policy
                                            :message "Unsupported test keyword")))
           :filters (list (cons :parse-tree (list #'parse-tree-filter))))
          (setf registered-p t)
          (unwind-protect
              (with-soft-assertions
               (expect (run-export) :to-equal "rendered")
               (expect acyclic-result :to-be t)
               (let* ((accepted-title-keyword-p
                       (null (org-kit::export-validate-keywords
                              (list (list nil "TITLE" "")) :html)))
                      (accepted-org-keyword-p
                       (null (org-kit::export-validate-keywords
                              (list (list nil "CUSTOM_KEY" "Visible")) :org)))
                      (org-document (parse-org "#+CUSTOM_KEY: Retained"))
                      (org-filter-context
                       (org-kit::%make-export-render-context
                        :backend :org :document org-document :owner-backend :org))
                      (org-preparation
                       (org-kit::export-prepare
                        org-document :org :filter-context org-filter-context))
                      (org-keywords
                       (export-core-test-view-nodes org-preparation :keyword)))
                 (expect accepted-title-keyword-p :to-be t)
                 (expect accepted-org-keyword-p :to-be t)
                 (expect
                  (mapcar (lambda (view)
                            (node-property (org-kit::export-view-node view) :key))
                          org-keywords)
                  :to-equal (list "CUSTOM_KEY")))
               (let* ((preparation
                       (org-kit::export-prepare
                        (parse-org "#+TITLE: Retained") backend))
                      (keywords
                       (export-core-test-view-nodes preparation :keyword)))
                 (expect (mapcar (lambda (view)
                                   (node-property
                                    (org-kit::export-view-node view) :key))
                                 keywords)
                         :to-equal '("TITLE")))
               (progn
                 (expect (typep :not-an-export-view
                                'org-kit::acyclic-export-view)
                         :to-be nil)
                 (expect
                  (typep
                   (export-core-test-catch-error
                    (lambda ()
                      (org-kit::export-check-output-stage
                       "" (make-limits) backend :body :actual-offset "not-an-integer")))
                   'type-error)
                  :to-be t)
                 (expect (org-kit::export-global-keyword-p "TITLE") :to-be-truthy)
                 (expect (org-kit::export-global-keyword-p "CAPTION") :to-be nil)
                 (expect (org-kit::export-explicit-options-alist-p '(:toc)) :to-be nil)
                 (expect
                  (org-kit::export-explicit-options-alist-p '(("toc" . t)))
                  :to-be nil)
                 (expect
                  (org-kit::export-explicit-options-alist-p '(:toc . :invalid))
                  :to-be nil)
                 (multiple-value-bind (descriptor ancestry)
                     (org-kit::export-registry-snapshot :html)
                   (declare (ignore descriptor))
                   (expect
                    (org-kit::export-valid-explicit-options-alist-p
                     '(:toc . :invalid) ancestry)
                    :to-be nil)
                   (expect
                    (org-kit::export-valid-explicit-options-alist-p
                     '((:not-a-registered-option . t)) ancestry)
                    :to-be nil)
                   (let* ((document (parse-org "Body"))
                          (context
                            (org-kit::%make-export-render-context
                             :backend :html :document document :ancestry ancestry
                             :owner-backend :html))
                          (preparation
                            (org-kit::export-prepare
                             document :html :filter-context context)))
                     (expect (typep preparation 'org-kit::export-preparation) :to-be t)
                     (expect (eq (org-kit::%export-render-context-preparation context)
                                 preparation)
                             :to-be t))))
               (dolist (invalid-mode '(:invalid-root :invalid-view :invalid-node
                                                     :invalid-children :invalid-child
                                                     :invalid-source :cycle))
                 (setf mode invalid-mode)
                 (expect (typep (export-core-test-catch-error #'run-export)
                                'type-error)
                         :to-be t)
                 (when (member invalid-mode '(:invalid-children :invalid-child
                                                                :cycle))
                   (expect acyclic-result :to-be nil)))
               (setf mode :deep)
               (expect
                (typep
                 (export-core-test-catch-error
                  (lambda ()
                    (run-export (make-limits :max-element-depth 4))))
                 'org-resource-limit-error)
                :to-be t)
               (setf mode :too-many)
               (expect
                (typep
                 (export-core-test-catch-error
                  (lambda ()
                    (run-export (make-limits :max-export-expanded-nodes 10))))
                 'org-resource-limit-error)
                :to-be t)
               (let ((invalid-document
                      (export-core-test-catch-error
                       (lambda () (export-org nil backend))))
                     (invalid-backend
                      (export-core-test-catch-error
                       (lambda () (export-org (parse-org "Body") "html"))))
                     (invalid-resolver
                      (export-core-test-catch-error
                       (lambda ()
                         (export-org (parse-org "Body") backend
                                     :include-resolver :not-a-function))))
                     (invalid-stage-text
                      (export-core-test-catch-error
                       (lambda ()
                         (org-kit::export-check-output-stage
                          nil (make-limits) backend :body))))
                     (invalid-stage-offset
                      (export-core-test-catch-error
                       (lambda ()
                         (org-kit::export-check-output-stage
                          "" (make-limits) backend :body :actual-offset -1)))))
                 (progn
                   (dolist (condition (list invalid-document invalid-backend
                                             invalid-resolver invalid-stage-text
                                             invalid-stage-offset))
                     (expect (typep condition 'type-error) :to-be t))
                   (let ((unknown-backend-error
                           (export-core-test-catch-error
                            (lambda ()
                              (export-org
                               (parse-org "Body")
                               :api-export-core-unregistered-with-invalid-resolver
                               :include-resolver :not-a-function)))))
                     (expect (typep unknown-backend-error 'org-export-error)
                             :to-be t)
                     (when (typep unknown-backend-error 'org-export-error)
                       (expect (org-export-error-backend unknown-backend-error)
                               :to-be :api-export-core-unregistered-with-invalid-resolver)
                       (expect (org-export-error-reason unknown-backend-error)
                               :to-be :unknown-backend))))))
            (when registered-p
              (unregister-export-backend backend)))))
      ))
(progn
  (defun export-core-test-oracle-fixture (relative-path)
    (uiop:read-file-string
     (asdf:system-relative-pathname
      "cl-org-kit"
      (concatenate 'string "t/fixtures/export/oracle/" relative-path))))

  (describe "X1b oracle-backed export preparation"
    (it "FR-071 retains backend keyword and OPTIONS values"
      (let* ((html-options
               (org-kit::export-preparation-options
                (org-kit::export-prepare
                 (parse-org (export-core-test-oracle-fixture
                             "corpus/fr071-html-settings.org")) :html)))
             (latex-options
               (org-kit::export-preparation-options
                (org-kit::export-prepare
                 (parse-org (export-core-test-oracle-fixture
                             "corpus/fr071-latex-settings.org")) :latex)))
             (invalid-option
               (export-core-test-catch-error
                (lambda ()
                  (org-kit::export-prepare
                   (parse-org "#+OPTIONS: html5-fancy:maybe") :html)))))
        (with-soft-assertions
         (expect (mapcar (lambda (key) (cdr (assoc key html-options)))
                         '(:html-doctype :html-container :html-content-class
                                         :description :keywords :html-link-home :html-link-up
                                         :html-equation-reference-format :html-head
                                         :html-head-extra :subtitle :infojs-opt :creator
                                         :html-html5-fancy :html-link-use-abs-url
                                         :html-postamble :html-preamble
                                         :html-head-include-default-style
                                         :html-head-include-scripts :html-self-link-headlines))
                 :to-equal
                 (list "html5" "main" "report"
                       (format nil "first description~%second description")
                       "alpha beta" "/" "../" "last"
                       (format nil "head one~%head two")
                       (format nil "extra one~%extra two")
                       "Export guide" "toc:1" "Writer"
                       t nil :auto t nil t nil))
         (expect (mapcar (lambda (key) (cdr (assoc key latex-options)))
                         '(:latex-class :latex-class-options :latex-header
                                        :latex-header-extra :latex-class-pre :description
                                        :keywords :subtitle :latex-default-footnote-command
                                        :latex-engraved-theme :latex-compiler :latex-use-sans))
                 :to-equal
                 (list "report" "twoside"
                       (format nil "one header~%two header")
                       (format nil "extra one~%extra two")
                       (format nil "pre one~%pre two")
                       (format nil "first description~%second description")
                       (format nil "alpha~%beta") "Export guide" "footnote"
                       "modus-operandi" "xelatex" t))
         (expect (typep invalid-option 'org-export-unsupported-syntax) :to-be t)
         (expect
          (cdr
           (assoc :creator
                  (org-kit::export-preparation-options
                   (org-kit::export-prepare
                    (parse-org
                     (export-core-test-lines
                      "#+OPTIONS: creator:comment"
                      "#+CREATOR: Writer"
                      "Body"))
                    :html))))
          :to-equal "Writer")
         (when (typep invalid-option 'org-export-unsupported-syntax)
           (expect (org-export-unsupported-syntax-feature invalid-option)
                   :to-be :invalid-option)))))

    (it "FR-075 retains TOC directives in source order and rejects unsupported modes"
      (let* ((document (parse-org (export-core-test-oracle-fixture
                                   "corpus/fr075-toc-directives.org")))
             (preparation (org-kit::export-prepare document :html))
             (options (org-kit::export-preparation-options preparation))
             (views (export-core-test-view-nodes preparation :keyword))
             (nodes (mapcar #'org-kit::export-view-node views))
             (invalid-mode
               (export-core-test-catch-error
                (lambda ()
                  (export-org (parse-org "#+TOC: unsupported") :html)))))
        (with-soft-assertions
          (expect (cdr (assoc :toc options)) :to-be 3)
          (expect (mapcar (lambda (node) (node-property node :key)) nodes)
                  :to-equal '("TOC" "TOC" "TOC" "TOC"))
          (expect (mapcar (lambda (node) (node-property node :value)) nodes)
                  :to-equal '("headlines 2" "local" "tables" "listings"))
          (expect (every (lambda (view)
                           (eq (org-kit::export-view-source-document view) document))
                         views)
                  :to-be t)
          (expect (typep invalid-mode 'org-export-error) :to-be t)
          (when (typep invalid-mode 'org-export-error)
            (expect (org-export-error-reason invalid-mode) :to-be :toc-mode)))))

    (it "FR-072 matches the Org 9.8.10 INCLUDE oracle outputs"
      (let ((cases
              '(("fr072-lines-exclusive" ("included/fragment.org"))
                ("fr072-lines-to-eof" ("included/fragment.org"))
                ("fr072-minlevel-default" ("included/fragment.org"))
                ("fr072-minlevel-explicit" ("included/fragment.org"))
                ("fr072-target-and-literal"
                 ("included/targets.org" "included/targets.org"
                  "included/targets.org" "included/targets.org"
                  "included/literal.org" "included/literal.org"
                  "included/literal.org")))))
        (with-soft-assertions
          (dolist (case cases)
            (destructuring-bind (name expected-calls) case
              (let* ((source-path
                       (format nil "t/fixtures/export/oracle/corpus/~A.org" name))
                     (document
                       (parse-org (export-core-test-oracle-fixture
                                   (format nil "corpus/~A.org" name))
                                  :source-name source-path))
                     (expected (export-core-test-oracle-fixture
                               (format nil "ascii/~A.txt" name)))
                     (calls nil)
                     (actual
                       (export-org
                        document :ascii
                        :include-resolver
                        (lambda (path parent-source)
                          (declare (ignore parent-source))
                          (push path calls)
                          (export-core-test-oracle-fixture
                           (concatenate 'string "corpus/" path))))))
                (expect actual :to-equal expected)
                (expect (nreverse calls) :to-equal expected-calls)))))))

    (it "FR-073 matches DATE, file, property, counter, results, and index oracles"
      (let* ((source-path "t/fixtures/export/oracle/corpus/fr073-macro-expansion.org")
             (document (parse-org (export-core-test-oracle-fixture
                                   "corpus/fr073-macro-expansion.org")
                                  :source-name source-path))
             (registry (make-callback-registry))
             (calls nil)
             (actual
               (progn
                 (register-callback
                  registry :evaluator :emacs-lisp
                  (lambda (expression macro-context)
                    (push (list expression macro-context) calls)
                    (if (getf macro-context :arguments)
                        "2024-03-04"
                        "<2024-03-04 Mon>")))
                 (export-org document :ascii
                             :context (make-export-context :date "2050-01-01")
                             :callback-registry registry)))
             (ordered-calls (nreverse calls))
             (expected (export-core-test-oracle-fixture
                        "ascii/fr073-macro-expansion.txt"))
             (unsupported-id
               (export-core-test-catch-error
                (lambda ()
                  (export-core-test-preparation
                   (export-core-test-lines "* Heading" ":PROPERTIES:"
                                           ":VALUE: stored" ":ID: id-value"
                                           ":END:"
                                           "{{{property(VALUE,id:id-value)}}}")))))
             (override
               (export-core-test-preparation
                (export-core-test-lines "#+MACRO: results Local $1"
                                         "{{{results(value)}}}")))
             (override-text
               (getf (org-kit::export-view-payload
                      (first (export-core-test-view-nodes override :macro)))
                     :expanded-text)))
        (with-soft-assertions
          (expect actual :to-equal expected)
          (expect (length ordered-calls) :to-be 2)
          (expect (mapcar (lambda (call) (getf (second call) :macro-name))
                          ordered-calls)
                  :to-equal '("date" "date"))
          (expect (mapcar (lambda (call) (getf (second call) :arguments))
                          ordered-calls)
                  :to-equal '(("%Y-%m-%d") nil))
          (expect (every (lambda (call)
                           (and (search "org-format-timestamp" (first call))
                                (search "<2024-03-04 Mon>" (first call))))
                         ordered-calls)
                  :to-be t)
          (expect (typep unsupported-id 'org-export-unsupported-syntax) :to-be t)
          (when (typep unsupported-id 'org-export-unsupported-syntax)
            (expect (org-export-unsupported-syntax-feature unsupported-id)
                    :to-be :macro-property-location))
          (expect override-text :to-equal "Local value"))))

    (it "FR-074 exposes the source and recursively expanded keyword records"
      (let* ((document
               (parse-org
                (export-core-test-lines "#+SETUPFILE: nested.org"
                                        "#+TITLE: Main title"
                                        "#+LANGUAGE: ja")
                :source-name "manual/main.org"))
             (source-values
               (mapcar (lambda (record) (list (second record) (third record)))
                       (org-kit::export-keyword-records document)))
             (records
               (nth-value
                0
                (org-kit::export-setupfile-keyword-records
                 document :html
                 (lambda (path parent-source)
                   (declare (ignore parent-source))
                   (if (string= path "nested.org") "#+TITLE: Nested title" ""))
                 (document-limits document)
                 (org-kit::export-node-count (document-root document))
                 0)))
             (expanded-values
               (mapcar (lambda (record) (list (second record) (third record)))
                       (remove-if (lambda (record)
                                    (string= (second record) "SETUPFILE"))
                                  records))))
        (with-soft-assertions
          (expect source-values
                  :to-equal '(("SETUPFILE" "nested.org")
                              ("TITLE" "Main title")
                              ("LANGUAGE" "ja")))
          (expect expanded-values
                  :to-equal '(("TITLE" "Nested title")
                              ("TITLE" "Main title")
                              ("LANGUAGE" "ja"))))))

    (it "FR-090 delegates the function macro to its registered evaluator"
      (let* ((document (parse-org (export-core-test-oracle-fixture
                                   "corpus/fr090-function-macro.org")))
             (expected (export-core-test-oracle-fixture
                        "ascii/fr090-function-macro.txt"))
             (registry (make-callback-registry))
             (calls nil)
             (actual
               (progn
                 (register-callback
                  registry :evaluator :emacs-lisp
                  (lambda (expression macro-context)
                    (push (list expression macro-context) calls)
                    "delegated"))
                 (export-org document :ascii :callback-registry registry)))
             (call (first calls)))
        (with-soft-assertions
          (expect actual :to-equal expected)
          (expect (getf (second call) :macro-name) :to-equal "runtime")
          (expect (getf (second call) :arguments) :to-equal nil)
          (expect (search "(eval" (first call)) :to-be-truthy))))))
