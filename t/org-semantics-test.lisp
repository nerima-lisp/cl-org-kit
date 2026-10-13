;;;; t/org-semantics-test.lisp
(in-package #:cl-org-kit/test)

(describe "Org setting and heading semantics"
  (it "resolves source setting names to document settings keys"
    (let* ((document
             (parse-org (format nil "#+TODO: TODO | DONE~%#+STARTUP: odd~%#+LINK: home /home~%")
                        :settings '(:category "Notes"
                                    :file-tags ("work")
                                    :inlinetask-min-level 15
                                    :link-abbrevs (("manual" . "https://example.test"))
                                    :odd-levels-only-p t
                                    :tag-alist ((:tag "work" :fast-key nil)))))
           (todo (org-kit::org-setting-value document :todo-keywords))
           (startup (org-kit::org-setting-value document :startup))
           (link-abbrevs (org-kit::org-setting-value document :link-abbrevs))
           (tag-alist (org-kit::org-setting-value document :tag-alist)))
      (with-soft-assertions
        (expect (mapcar (lambda (key) (org-kit::org-setting-value document key))
                        '("TODO" "seq_todo" "TYP_TODO" "TODO[1]"
                          "SEQ_TODO[2]" "TYP_TODO[3]" :startup "category"
                          "FILETAGS" "INLINETASK-MIN-LEVEL" "LINK"
                          "LINK-ABBREVS" "ODD-LEVELS-ONLY-P" "TAG-ALIST"
                          :tags "TAGS" "TODO-KEYWORDS" :todo-keywords))
                :to-equal
                (list todo todo todo todo todo todo startup "Notes" '("work")
                      15 link-abbrevs link-abbrevs t tag-alist tag-alist
                      tag-alist todo todo))
        (expect (org-kit::org-setting-value document "TODO[x" :fallback)
                :to-equal :fallback)
        (expect (org-kit::org-setting-value document "unknown" :fallback)
                :to-equal :fallback)
        (expect (org-kit::org-setting-value document "FILETAGS" :fallback)
                :to-equal '("work")))))

  (it "returns raw settings, a default for unknown keys, and derived odd mode"
    (let* ((document
            (parse-org (format nil "#+STARTUP: odd~%")
                       :settings '(:category "Notes")))
           (unknown-default
             (org-kit::org-setting-value document "unknown" :fallback)))
      (with-soft-assertions
        (expect (org-kit::org-setting-value document :startup)
                :to-equal '("odd"))
        (expect (org-kit::org-setting-value document "category")
                :to-equal "Notes")
        (expect (org-kit::org-setting-value document :odd-levels-only-p)
                :to-be t)
        (expect unknown-default :to-equal :fallback)
        (expect (org-kit::org-setting-value document "FILETAGS" :fallback)
                :to-equal :fallback))))

  (it "uses the last recognized odd or oddeven startup option"
    (let ((document
            (parse-org (format nil "#+STARTUP: odd~%#+STARTUP: oddeven~%"))))
      (with-soft-assertions
        (expect (org-kit::org-setting-value document :startup)
                :to-equal '("odd" "oddeven"))
        (expect (org-kit::org-setting-value document :odd-levels-only-p)
                :to-be nil)))
    (let ((document (parse-org (format nil "#+STARTUP: oddeven odd showall~%"))))
      (expect (org-kit::org-setting-value document :odd-levels-only-p)
              :to-be t))
    (let ((document (parse-org (format nil "#+STARTUP: showall~%"))))
      (expect (org-kit::org-setting-value document :odd-levels-only-p)
              :to-be nil)))

  (it "lets an explicit odd-level setting override startup tokens"
    (let ((enabled (parse-org "#+STARTUP: oddeven"
                              :settings '(:odd-levels-only-p t)))
          (disabled (parse-org "#+STARTUP: odd"
                               :settings '(:odd-levels-only-p nil))))
      (with-soft-assertions
        (expect (org-kit::org-setting-value enabled :odd-levels-only-p)
                :to-be t)
        (expect (org-kit::org-setting-value disabled :odd-levels-only-p)
                :to-be nil))))

  (it "maps source star counts and logical levels in default mode"
    (let ((document (parse-org "")))
      (with-soft-assertions
        (expect (mapcar (lambda (stars)
                          (org-kit::logical-heading-level document stars))
                        '(1 2 3 4))
                :to-equal '(1 2 3 4))
        (expect (mapcar (lambda (level)
                          (org-kit::source-heading-level document level))
                        '(1 2 3 4))
                :to-equal '(1 2 3 4)))))

  (it "maps odd startup levels and returns canonical source star counts"
    (let ((document
            (parse-org (format nil "#+STARTUP: odd~%"))))
      (with-soft-assertions
        (expect (mapcar (lambda (stars)
                          (org-kit::logical-heading-level document stars))
                        '(1 2 3 4 5 6))
                :to-equal '(1 2 2 3 3 4))
        (expect (mapcar (lambda (level)
                          (org-kit::source-heading-level document level))
                        '(1 2 3 4))
                :to-equal '(1 3 5 7))
        (expect (mapcar (lambda (level)
                          (org-kit::logical-heading-level
                           document (org-kit::source-heading-level document level)))
                        '(1 2 3 4))
                :to-equal '(1 2 3 4))))))

(defun org-semantics-test-headings (document)
  "Return DOCUMENT's headlines in source order."
  (let ((headings nil))
    (map-nodes (lambda (node)
                 (when (member (node-type node) '(:headline :inlinetask) :test #'eq)
                   (push node headings)))
               (document-root document))
    (nreverse headings)))

(defun org-semantics-test-read-tree (path)
  "Read the one S-expression stored at PATH without evaluating reader forms."
  (with-open-file (stream path :direction :input)
    (let ((*read-eval* nil))
      (read stream))))

(describe "Effective heading tags and properties"
  (it "matches Org's nearest-position tag oracle and selector modes"
    (let* ((source-path
             (asdf:system-relative-pathname
              "cl-org-kit" "t/fixtures/corpus/settings/effective-tags-duplicates.org"))
           (expected-path
             (asdf:system-relative-pathname
              "cl-org-kit" "t/fixtures/expected/settings/effective-tags-duplicates.org.sexp"))
           (source (uiop:read-file-string source-path))
           (expected (org-semantics-test-read-tree expected-path))
           (expected-parent
             (find :headline (cddr expected) :key #'first))
           (expected-child
             (find :headline (cddr expected-parent) :key #'first))
           (document (parse-org source))
           (headings (org-semantics-test-headings document))
           (parent (first headings))
           (child (second headings))
           (before (document-text document)))
      (with-soft-assertions
        (expect (effective-heading-tags document parent)
                :to-equal (getf (second expected-parent) :effective-tags))
        (expect (effective-heading-tags document child)
                :to-equal (getf (second expected-child) :effective-tags))
        (expect (effective-heading-tags document child :inherit nil)
                :to-equal '("child_before" "shared" "child_after"))
        (expect (effective-heading-tags document child
                                        :inherit '("shared" "parent_before"))
                :to-equal '("parent_before" "child_before" "shared" "child_after"))
        (expect (effective-heading-tags document child
                                        :inherit '(:regexp "^file_"))
                :to-equal '("file_before" "file_after" "child_before"
                            "shared" "child_after"))
        (expect (effective-heading-tags document child
                                        :inherit '(:exclude ("shared")
                                                   (:regexp "^file_")))
                :to-equal '("parent_before" "parent_after" "child_before"
                            "shared" "child_after"))
        (expect (effective-heading-tags document child
                                        :inherit '(:exclude ("absent")))
                :to-equal '("file_before" "file_after" "parent_before"
                            "parent_after" "child_before" "shared"
                            "child_after"))
        (expect (effective-heading-tags document child
                                        :inherit '(:exclude "absent"))
                :to-equal '("file_before" "file_after" "parent_before"
                            "parent_after" "child_before" "shared"
                            "child_after"))
        (expect (document-text document) :to-equal before))))

  (it "resolves direct, inherited, appended, and document properties"
    (let* ((document
             (parse-org
              (format nil
                      "#+PROPERTY: FALLBACK global-value~%#+PROPERTY: X global~%* Root :ancestor:~%:PROPERTIES:~%:DESCRIPTION~%:INHERITED: root-value~%:SHARED: root-value~%:LAYERED: root-direct~%:LAYERED+: root-plus~%:SEQUENCE+: root-plus~%:X+: root-plus~%:END:~%** Child :child:~%:PROPERTIES:~%:LOCAL: child-value~%:SHARED: child-value~%:SEQUENCE: child-direct~%:SEQUENCE+: child-plus~%:LAYERED+: child-plus~%:X+: child-plus~%:END:~%*** Grandchild~%:PROPERTIES:~%:SEQUENCE+: grandchild-plus~%:LAYERED+: grandchild-plus~%:X+: grandchild-plus~%:END:~%")))
           (headings (org-semantics-test-headings document))
           (root (first headings))
           (child (second headings))
           (grandchild (third headings))
           (before (document-text document)))
      (with-soft-assertions
        (multiple-value-bind (value source)
            (effective-property document grandchild "LOCAL" :inherit t)
          (expect value :to-equal "child-value")
          (expect source :to-be child))
        (multiple-value-bind (value source)
            (effective-property document grandchild "INHERITED" :inherit t)
          (expect value :to-equal "root-value")
          (expect source :to-be root))
        (multiple-value-bind (value source)
            (effective-property document grandchild "SHARED" :inherit t)
          (expect value :to-equal "child-value")
          (expect source :to-be child))
        (multiple-value-bind (value source)
            (effective-property document grandchild "LOCAL")
          (expect value :to-be nil)
          (expect source :to-be nil))
        (multiple-value-bind (value source)
            (effective-property document grandchild "INHERITED"
                                :inherit '("INHERITED"))
          (expect value :to-equal "root-value")
          (expect source :to-be root))
        (multiple-value-bind (value source)
            (effective-property document grandchild "INHERITED"
                                :inherit '(:regexp "^INHERITED$"))
          (expect value :to-equal "root-value")
          (expect source :to-be root))
        (multiple-value-bind (value source)
            (effective-property document grandchild "INHERITED"
                                :inherit '(:exclude UNRELATED))
          (expect value :to-equal "root-value")
          (expect source :to-be root))
        (multiple-value-bind (value source)
            (effective-property document grandchild "INHERITED"
                                :inherit '(:exclude ("INHERITED")))
          (expect value :to-be nil)
          (expect source :to-be nil))
        (multiple-value-bind (value source)
            (effective-property document grandchild "FALLBACK")
          (expect value :to-be nil)
          (expect source :to-be nil))
        (multiple-value-bind (value source)
            (effective-property document grandchild "FALLBACK" :inherit t)
          (expect value :to-equal "global-value")
          (expect source :to-be :document))
        (multiple-value-bind (value source)
            (effective-property document grandchild "X" :inherit t)
          (expect value :to-equal
                  (format nil "global~%root-plus~%child-plus~%grandchild-plus"))
          (expect source :to-equal (list :document root child grandchild)))
        (multiple-value-bind (value source)
            (effective-property document grandchild "X")
          (expect value :to-equal "grandchild-plus")
          (expect source :to-equal (list grandchild)))
        (multiple-value-bind (value source)
            (effective-property document grandchild "SHARED"
                                :inherit '(SHARED))
          (expect value :to-equal "child-value")
          (expect source :to-be child))
        (multiple-value-bind (value source)
            (effective-property document grandchild 'LOCAL :inherit t)
          (expect value :to-equal "child-value")
          (expect source :to-be child))
        (multiple-value-bind (value source)
            (effective-property document grandchild "SEQUENCE" :inherit t)
          (expect value :to-equal
                  (format nil "child-direct~%child-plus~%grandchild-plus"))
          (expect source :to-equal (list child grandchild)))
        (multiple-value-bind (value source)
            (effective-property document grandchild "LAYERED" :inherit t)
          (expect value :to-equal
                  (format nil "root-direct~%root-plus~%child-plus~%grandchild-plus"))
          (expect source :to-equal (list root child grandchild)))
        (expect (document-text document) :to-equal before))))

  (it "accepts inlinetasks as headings for effective semantics"
    (let* ((source
             (format nil "*************** Inline :inline_tag:~%:PROPERTIES:~%:INLINE-PROP: inline-value~%:END:~%Inline body.~%*************** END~%"))
           (document (parse-org source))
           (inline
             (first (org-semantics-test-headings document))))
      (with-soft-assertions
        (expect (node-type inline) :to-be :inlinetask)
        (expect (effective-heading-tags document inline :inherit nil)
                :to-equal '("inline_tag"))
        (multiple-value-bind (value source)
            (effective-property document inline "INLINE-PROP")
          (expect value :to-equal "inline-value")
          (expect source :to-be inline)))))

  (it "reports ownership, type, and selector errors without changing input"
    (let* ((source (format nil "* Heading~%"))
           (document (parse-org source))
           (foreign-document (parse-org source))
           (heading (first (org-semantics-test-headings document)))
           (foreign-heading (first (org-semantics-test-headings foreign-document)))
           (before (document-text document)))
      (expect
       (handler-case
           (progn (effective-heading-tags document foreign-heading) nil)
         (org-edit-error (condition)
           (expect (org-edit-error-reason condition) :to-be :foreign-node)
           (expect (stringp (org-edit-error-target condition)) :to-be t)
           t))
       :to-be t)
      (expect
       (handler-case
           (progn (effective-property document (document-root document) "NAME") nil)
         (org-edit-error (condition)
           (expect (org-edit-error-reason condition) :to-be :wrong-node-type)
           (expect (stringp (org-edit-error-target condition)) :to-be t)
           t))
       :to-be t)
      (expect
       (handler-case
           (progn (effective-heading-tags document heading :inherit :invalid) nil)
         (org-query-error (condition)
           (expect (org-query-error-query condition) :to-equal ":INVALID")
           (expect (org-query-error-position condition) :to-be 0)
           t))
       :to-be t)
      (expect
       (handler-case
           (progn (effective-heading-tags document heading :inherit '(:regexp "(")) nil)
         (org-query-error (condition)
           (expect (org-query-error-query condition)
                   :to-equal "(:REGEXP \"(\")")
           (expect (integerp (org-query-error-position condition)) :to-be t)
           t))
       :to-be t)
      (signals type-error
        (effective-heading-tags :not-a-document heading))
      (expect
       (handler-case
           (progn (effective-heading-tags document nil) nil)
         (org-edit-error (condition)
           (expect (org-edit-error-reason condition) :to-be :target-not-found)
           t))
       :to-be t)
      (signals org-query-error
        (effective-heading-tags document heading :inherit '(42)))
      (signals org-query-error
        (effective-heading-tags document heading :inherit "shared"))
      (signals org-query-error
        (effective-heading-tags document heading
                                :inherit '(:exclude (:regexp))))
      (signals org-query-error
        (effective-heading-tags document heading
                                :inherit '(:exclude (:exclude . malformed))))
      (signals org-query-error
        (effective-heading-tags document heading :inherit '(:exclude 42)))
      (signals org-query-error
        (effective-heading-tags document heading
                                :inherit '(:regexp 42)))
      (signals type-error
        (effective-property document heading 42))
      (expect (document-text document) :to-equal before))))

(describe "Effective document properties"
  (it "uses the last document property declaration"
    (let* ((document
             (parse-org
              (format nil
                      "#+PROPERTY: RESULT first~%#+PROPERTY: RESULT second~%* Heading~%")))
           (heading (first (org-semantics-test-headings document))))
      (multiple-value-bind (value source)
          (effective-property document heading "RESULT" :inherit t)
        (expect value :to-equal "second")
        (expect source :to-be :document))))

  (it "skips malformed global property entries while resolving fallback"
    (let* ((document
             (parse-org "* Heading"
                        :settings
                        '(:property-settings (42 ("RESULT" . "global-value")))))
           (heading (first (org-semantics-test-headings document))))
      (multiple-value-bind (value source)
          (effective-property document heading "RESULT" :inherit t)
        (expect value :to-equal "global-value")
        (expect source :to-be :document)))))
