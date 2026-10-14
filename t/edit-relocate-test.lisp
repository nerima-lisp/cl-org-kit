;;;; t/edit-relocate-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-relocate-test-fixture (relative-path)
  (let ((path (asdf:system-relative-pathname
               "cl-org-kit" relative-path))
        (eof (gensym "EOF")))
    (with-open-file (stream path :direction :input)
      (let ((*read-eval* nil)
            (fixture (read stream nil eof)))
        (when (eq fixture eof)
          (error "Relocation fixture ~A is empty." relative-path))
        (unless (eq (read stream nil eof) eof)
          (error "Relocation fixture ~A has more than one form."
                 relative-path))
        fixture))))

(defun edit-relocate-test-headline (document title)
  (let ((found nil))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :headline)
                  (string= title
                           (node-property node :raw-value)))
         (setf found node)))
     (document-root document))
    found))

(defun edit-relocate-test-span-data (result)
  (mapcar (lambda (span)
            (list (edit-span-start span) (edit-span-end span)))
          (edit-result-spans result)))

(defun edit-relocate-test-property-value (source name)
  (let* ((prefix (format nil ":~A: " name))
         (start (search prefix source)))
    (when start
      (let* ((value-start (+ start (length prefix)))
             (end (or (position #\Newline source :start value-start)
                      (length source))))
        (subseq source value-start end)))))

(defun edit-relocate-test-edit-error (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (org-edit-error (condition) condition)))

(defun edit-relocate-test-limit-error (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (org-resource-limit-error (condition) condition)))

(describe "subtree relocation and archival"
  (it "preserves exact source spans and leaves both inputs unchanged on refile"
    (let* ((fixture (edit-relocate-test-fixture
                     "t/fixtures/e/relocate/refile-exact-spans.sexp"))
           (source-text (getf fixture :source-input))
           (target-text (getf fixture :target-input))
           (source (parse-org source-text))
           (target (parse-org target-text))
           (source-heading
             (edit-relocate-test-headline source (getf fixture :source-heading)))
           (target-heading
             (edit-relocate-test-headline target (getf fixture :target-heading)))
           (start (search "** Source" source-text))
           (end (search "* Tail" source-text))
           (target-offset (length target-text))
           (result (refile-subtree source source-heading target target-heading
                                   :mode (getf fixture :mode)))
           (source-edit (relocation-result-source-edit result))
           (target-edit (relocation-result-target-edit result))
           (updated-source (edit-result-updated-document source-edit))
           (updated-target (edit-result-updated-document target-edit)))
      (with-soft-assertions
        (expect (relocation-result-p result) :to-be t)
        (expect (relocation-result-mode result) :to-be :move)
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text)
        (expect (document-text updated-source)
                :to-equal (getf fixture :source-expected))
        (expect (document-text updated-target)
                :to-equal (getf fixture :target-expected))
        (expect (edit-relocate-test-span-data source-edit)
                :to-equal (list (list start end)))
        (expect (edit-relocate-test-span-data target-edit)
                :to-equal (list (list target-offset target-offset)))
        (expect (relocation-result-source-path result)
                :to-equal '((:title "Parent" :index 1)
                            (:title "Source" :index 1)))
        (expect (relocation-result-target-path result)
                :to-equal '((:title "Target" :index 1)
                            (:title "Source" :index 1)))
        (expect (node-property
                 (edit-relocate-test-headline updated-target "Source") :level)
                :to-be 2))))

  (it "matches the Org refile root and heading oracle fixtures"
    (dolist (relative-path
              '("t/fixtures/e/oracle/relocate/refile-root.sexp"
                "t/fixtures/e/oracle/relocate/refile-heading.sexp"))
      (let* ((fixture (edit-relocate-test-fixture relative-path))
             (source-text (getf fixture :source-input))
             (target-text (getf fixture :target-input))
             (source (parse-org source-text))
             (target (parse-org target-text))
             (source-heading (edit-relocate-test-headline source "Source"))
             (target-heading
               (and (getf fixture :target-heading)
                    (edit-relocate-test-headline
                     target (getf fixture :target-heading))))
             (result (refile-subtree source source-heading target target-heading))
             (updated-source
               (edit-result-updated-document
                (relocation-result-source-edit result)))
             (updated-target
               (edit-result-updated-document
                (relocation-result-target-edit result))))
        (with-soft-assertions
          (expect (document-text source) :to-equal source-text)
          (expect (document-text target) :to-equal target-text)
          (expect (document-text updated-source)
                  :to-equal (getf fixture :source-expected))
          (expect (document-text updated-target)
                  :to-equal (getf fixture :target-expected))
          (expect (relocation-result-mode result) :to-be :move)))))

  (it "refiles to an empty destination root"
    (let* ((source-text (format nil "* Source~%source body~%"))
           (source (parse-org source-text))
           (target (parse-org ""))
           (heading (edit-relocate-test-headline source "Source"))
           (result (refile-subtree source heading target nil))
           (updated-source
             (edit-result-updated-document
              (relocation-result-source-edit result)))
           (updated-target
             (edit-result-updated-document
              (relocation-result-target-edit result))))
      (with-soft-assertions
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal "")
        (expect (document-text updated-source) :to-equal "")
        (expect (document-text updated-target) :to-equal source-text)
        (expect (edit-relocate-test-span-data
                 (relocation-result-target-edit result))
                :to-equal '((0 0))))))

  (it "preserves a blank-only destination prefix when refileing at its root"
    (let* ((source-text (format nil "* Source~%"))
           (source (parse-org source-text))
           (blank (format nil "~%"))
           (target (parse-org blank))
           (result
             (refile-subtree source
                             (edit-relocate-test-headline source "Source")
                             target nil))
           (updated-target (edit-result-updated-document
                            (relocation-result-target-edit result))))
      (with-soft-assertions
        (expect (document-text updated-target)
                :to-equal (format nil "~%* Source~%"))
        (expect (document-text target) :to-equal blank))))

  (it "uses the target line ending when refiling a CRLF subtree to LF"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source-text (format nil "* Parent~A** Child~Abody~A"
                                crlf crlf crlf))
           (target-text (format nil "* Target~%"))
           (source (parse-org source-text))
           (target (parse-org target-text))
           (result (refile-subtree
                    source (edit-relocate-test-headline source "Child")
                    target (edit-relocate-test-headline target "Target")))
           (updated-source (edit-result-updated-document
                            (relocation-result-source-edit result)))
           (updated-target (edit-result-updated-document
                            (relocation-result-target-edit result))))
      (with-soft-assertions
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text)
        (expect (document-text updated-source) :to-equal (format nil "* Parent~A" crlf))
        (expect (document-text updated-target)
                :to-equal (format nil "* Target~%** Child~%body~%")))))

  (it "preserves a body carriage return while normalizing refiled LF separators"
    (let* ((standalone-cr (string #\Return))
           (source-text
             (format nil "* Parent~%** Child~%body~Acontinues~%"
                     standalone-cr))
           (target-text (format nil "* Target~%"))
           (source (parse-org source-text))
           (target (parse-org target-text))
           (result
             (refile-subtree source
                             (edit-relocate-test-headline source "Child")
                             target
                             (edit-relocate-test-headline target "Target")))
           (updated-source (edit-result-updated-document
                            (relocation-result-source-edit result)))
           (updated-target (edit-result-updated-document
                            (relocation-result-target-edit result)))
           (updated-text (document-text updated-target)))
      (with-soft-assertions
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text)
        (expect (document-text updated-source)
                :to-equal (format nil "* Parent~%"))
        (expect updated-text
                :to-equal
                (format nil "* Target~%** Child~%body~Acontinues~%"
                        standalone-cr))
        (expect (count #\Return updated-text) :to-be 1)
        (expect (count #\Newline updated-text) :to-be 3))))

  (it "preserves a trailing body carriage return while adding the target newline"
    (let* ((standalone-cr (string #\Return))
           (source-text
             (concatenate 'string (format nil "* Parent~%** Child~%body")
                          standalone-cr))
           (target-text (format nil "* Target~%"))
           (source (parse-org source-text))
           (target (parse-org target-text))
           (result
             (refile-subtree source
                             (edit-relocate-test-headline source "Child")
                             target nil))
           (updated-target (edit-result-updated-document
                            (relocation-result-target-edit result)))
           (updated-text (document-text updated-target)))
      (with-soft-assertions
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text)
        (expect updated-text
                :to-equal
                (concatenate 'string (format nil "* Target~%* Child~%body")
                             standalone-cr (string #\Newline)))
        (expect (count #\Return updated-text) :to-be 1))))

  (it "uses LF separators when refiling into a nonempty target without an EOL"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source-text (format nil "* Parent~A** Child~Abody~A"
                                crlf crlf crlf))
           (target-text "* Target")
           (source (parse-org source-text))
           (target (parse-org target-text))
           (result
             (refile-subtree source
                             (edit-relocate-test-headline source "Child")
                             target
                             (edit-relocate-test-headline target "Target")))
           (updated-source (edit-result-updated-document
                            (relocation-result-source-edit result)))
           (updated-target (edit-result-updated-document
                            (relocation-result-target-edit result)))
           (updated-text (document-text updated-target)))
      (with-soft-assertions
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text)
        (expect (document-text updated-source)
                :to-equal (format nil "* Parent~A" crlf))
        (expect updated-text
                :to-equal
                (format nil "* Target~%** Child~%body~%"))
        (expect (count #\Return updated-text) :to-be 0)
        (expect (count #\Newline updated-text) :to-be 3))))

  (it "copies a subtree without a source edit and keeps the source document intact"
    (let* ((source-text (format nil "* Source~%source body~%"))
           (target-text (format nil "* Target~%target body~%"))
           (source (parse-org source-text))
           (target (parse-org target-text))
           (source-heading (edit-relocate-test-headline source "Source"))
           (target-heading (edit-relocate-test-headline target "Target"))
           (result (refile-subtree source source-heading target target-heading
                                   :mode :copy))
           (target-edit (relocation-result-target-edit result)))
      (with-soft-assertions
        (expect (relocation-result-mode result) :to-be :copy)
        (expect (relocation-result-source-edit result) :to-be nil)
        (expect (document-text source) :to-equal source-text)
        (expect (document-text
                 (edit-result-updated-document target-edit))
                :to-equal (format nil "* Target~%target body~%** Source~%source body~%"))
        (expect (edit-relocate-test-span-data target-edit)
                :to-equal (list (list (length target-text) (length target-text)))))))

  (it "moves a subtree between same-document parents and reports both edits"
    (let* ((source-text
             (format nil "* One~%** Child~%child body~%* Two~%two body~%"))
           (document (parse-org source-text))
           (child (edit-relocate-test-headline document "Child"))
           (start (search "** Child" source-text))
           (end (search "* Two" source-text))
           (insert-offset (length source-text))
           (result (refile-subtree document child document nil))
           (source-edit (relocation-result-source-edit result))
           (target-edit (relocation-result-target-edit result))
           (updated (edit-result-updated-document target-edit)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source-text)
        (expect (edit-result-updated-document source-edit) :to-be updated)
        (expect (document-text updated)
                :to-equal (format nil "* One~%* Two~%two body~%* Child~%child body~%"))
        (expect (edit-relocate-test-span-data target-edit)
                :to-equal (list (list start end)
                                (list insert-offset insert-offset)))
        (expect (relocation-result-source-path result)
                :to-equal '((:title "One" :index 1)
                            (:title "Child" :index 1)))
        (expect (relocation-result-target-path result)
                :to-equal '((:title "Child" :index 1)))
        (expect (node-property
                 (edit-relocate-test-headline updated "Child") :level)
                :to-be 1))))

  (it "moves the immediately following sibling beneath its predecessor"
    (let* ((source-text
             (format nil
                     "* Parent~%** A~%A body~%** B~%B body~%** C~%C body~%"))
           (document (parse-org source-text))
           (a (edit-relocate-test-headline document "A"))
           (b (edit-relocate-test-headline document "B"))
           (result (refile-subtree document b document a))
           (source-edit (relocation-result-source-edit result))
           (target-edit (relocation-result-target-edit result))
           (updated-source (edit-result-updated-document source-edit))
           (updated-target (edit-result-updated-document target-edit)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source-text)
        (expect (document-text updated-target)
                :to-equal
                (format nil
                        "* Parent~%** A~%A body~%*** B~%B body~%** C~%C body~%"))
        (expect updated-source :to-be updated-target)
        (expect (relocation-result-source-path result)
                :to-equal '((:title "Parent" :index 1)
                            (:title "B" :index 1)))
        (expect (relocation-result-target-path result)
                :to-equal '((:title "Parent" :index 1)
                            (:title "A" :index 1)
                            (:title "B" :index 1)))
        (expect (node-property
                 (edit-relocate-test-headline updated-target "B") :level)
                :to-be 3))))

  (it "keeps a later root target path when moving across root parents"
    (let* ((source-text
             (format nil
                     "* One~%** Move~%move body~%* Two~%** Target~%target body~%"))
           (document (parse-org source-text))
           (moving (edit-relocate-test-headline document "Move"))
           (target (edit-relocate-test-headline document "Target"))
           (result (refile-subtree document moving document target))
           (updated (edit-result-updated-document
                     (relocation-result-target-edit result))))
      (with-soft-assertions
        (expect (document-text document) :to-equal source-text)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "* One~%* Two~%** Target~%target body~%*** Move~%move body~%"))
        (expect (relocation-result-target-path result)
                :to-equal '((:title "Two" :index 1)
                            (:title "Target" :index 1)
                            (:title "Move" :index 1)))
        (expect (node-property
                 (edit-relocate-test-headline updated "Move") :level)
                :to-be 3))))

  (it "adjusts a later root heading path after refile removes an earlier root"
    (let* ((source-text
             (format nil
                     "* Before~%before body~%* Move~%move body~%* Target~%target body~%"))
           (document (parse-org source-text))
           (moving (edit-relocate-test-headline document "Move"))
           (target (edit-relocate-test-headline document "Target"))
           (result (refile-subtree document moving document target))
           (updated (edit-result-updated-document
                     (relocation-result-target-edit result))))
      (with-soft-assertions
        (expect (document-text document) :to-equal source-text)
        (expect (document-text updated)
                :to-equal
                (format nil
                        "* Before~%before body~%* Target~%target body~%** Move~%move body~%"))
        (expect (relocation-result-target-path result)
                :to-equal '((:title "Target" :index 1)
                            (:title "Move" :index 1))))))

  (it "adjusts a nested target path after removing an earlier sibling"
    (let* ((source-text
             (format nil
                     "* Root~%** Parent~%*** Moving~%moving body~%*** Other~%**** Target~%target body~%"))
           (document (parse-org source-text))
           (result
             (refile-subtree document
                             (edit-relocate-test-headline document "Moving")
                             document
                             (edit-relocate-test-headline document "Target")))
           (updated (edit-result-updated-document
                     (relocation-result-target-edit result))))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal
                (format nil
                        "* Root~%** Parent~%*** Other~%**** Target~%target body~%***** Moving~%moving body~%"))
        (expect (relocation-result-target-path result)
                :to-equal '((:title "Root" :index 1)
                            (:title "Parent" :index 1)
                            (:title "Other" :index 1)
                            (:title "Target" :index 1)
                            (:title "Moving" :index 1)))
        (expect (document-text document) :to-equal source-text))))

  (it "rejects self and descendant relocation targets"
    (dolist (target-title '("Move" "Descendant"))
      (let* ((source-text
               (format nil "* Parent~%** Move~%*** Descendant~%"))
             (document (parse-org source-text))
             (moving (edit-relocate-test-headline document "Move"))
             (target (edit-relocate-test-headline document target-title))
             (condition
               (edit-relocate-test-edit-error
                (lambda () (refile-subtree document moving document target)))))
        (with-soft-assertions
          (expect (typep condition 'org-edit-error) :to-be t)
          (when condition
            (expect (org-edit-error-reason condition)
                    :to-be :uneditable-structure))
          (expect (document-text document) :to-equal source-text)))))

  (it "rejects relocation across affiliated and trailing-comment boundaries"
    (dolist (target-case
              (list (list (format nil
                                  "* Target~%target body~%#+CAPTION: Next~%* Next~%")
                          "Target")
                    (list (format nil "* Keep~%keep body~%# trailing~%") nil)))
      (let* ((source-text (format nil "* Source~%source body~%"))
             (target-text (first target-case))
             (source (parse-org source-text))
             (target (parse-org target-text))
             (target-heading
               (and (second target-case)
                    (edit-relocate-test-headline target
                                                 (second target-case))))
             (condition
               (edit-relocate-test-edit-error
                (lambda ()
                  (refile-subtree
                   source (edit-relocate-test-headline source "Source")
                   target target-heading)))))
        (with-soft-assertions
          (expect (typep condition 'org-edit-error) :to-be t)
          (when condition
            (expect (org-edit-error-reason condition)
                    :to-be :uneditable-structure))
          (expect (document-text source) :to-equal source-text)
          (expect (document-text target) :to-equal target-text)))))

  (it "rejects moving a source subtree across an affiliated next heading"
    (let* ((source-text
             (format nil
                     "* Move~%move body~%#+CAPTION: Next image~%* Next~%"))
           (source (parse-org source-text))
           (target (parse-org ""))
           (condition
             (edit-relocate-test-edit-error
              (lambda ()
                (refile-subtree source
                                (edit-relocate-test-headline source "Move")
                                target nil)))))
      (with-soft-assertions
        (expect (typep condition 'org-edit-error) :to-be t)
        (when condition
          (expect (org-edit-error-reason condition)
                  :to-be :uneditable-structure))
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal ""))))

  (it "relevels every descendant using the destination odd-level convention"
    (let* ((source-text (format nil "** Moving~%*** Nested~%"))
           (target-text (format nil "#+STARTUP: odd~%* Parent~%"))
           (source (parse-org source-text))
           (target (parse-org target-text))
           (heading (edit-relocate-test-headline source "Moving"))
           (parent (edit-relocate-test-headline target "Parent"))
           (result (refile-subtree source heading target parent))
           (updated (edit-result-updated-document
                     (relocation-result-target-edit result))))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal (format nil "#+STARTUP: odd~%* Parent~%*** Moving~%***** Nested~%"))
        (expect (node-property
                 (edit-relocate-test-headline updated "Moving") :level)
                :to-be 2)
        (expect (node-property
                 (edit-relocate-test-headline updated "Nested") :level)
                :to-be 3))))

  (it "swaps adjacent sibling subtrees and changes only their combined span"
    (let* ((source-text
             (format nil "* First~%first body~%* Second~%second body~%"))
           (document (parse-org source-text))
           (first (edit-relocate-test-headline document "First"))
           (result (move-subtree document first :direction :down))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (edit-result-p result) :to-be t)
        (expect (document-text document) :to-equal source-text)
        (expect (document-text updated)
                :to-equal (format nil "* Second~%second body~%* First~%first body~%"))
        (expect (edit-relocate-test-span-data result)
                :to-equal (list (list 0 (length source-text)))))))

  (it "moves a final sibling without adding a final newline"
    (let* ((source-text (format nil "* A~%* B"))
           (document (parse-org source-text))
           (result (move-subtree document
                                 (edit-relocate-test-headline document "B")
                                 :direction :up))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text updated) :to-equal (format nil "* B~%* A"))
        (expect (document-text document) :to-equal source-text))))

  (it "records Org's incompatible TODO preservation and rejects it by contract"
    (let* ((fixture (edit-relocate-test-fixture
                     "t/fixtures/e/oracle/relocate/refile-incompatible-todo.sexp"))
           (policy (edit-relocate-test-fixture
                    "t/fixtures/e/relocate/refile-incompatible-todo-policy.sexp"))
           (source-text (getf fixture :source-input))
           (target-text (getf fixture :target-input))
           (source (parse-org source-text))
           (target (parse-org target-text))
           (condition
             (edit-relocate-test-edit-error
              (lambda ()
                (refile-subtree source
                                (edit-relocate-test-headline source "Source")
                                target
                                (edit-relocate-test-headline target "Target"))))))
      (with-soft-assertions
        (expect (getf policy :divergence)
                :to-be :org-preserves-incompatible-todo)
        (expect (getf fixture :org-version) :to-equal "9.8.10")
        (expect (getf fixture :source-expected)
                :to-equal (format nil "#+TODO: WAIT | CLOSED~%"))
        (expect (getf fixture :target-expected)
                :to-equal (format nil
                                  "#+TODO: TODO | DONE~%* Target~%target body~%** WAIT Source~%source body~%"))
        (expect (typep condition 'org-edit-error) :to-be t)
        (when condition
          (expect (org-edit-error-reason condition)
                  :to-be (getf policy :contract-reason)))
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text))))

  (it "returns no partial cross-document move when destination output exceeds its limit"
    (let* ((source-text (format nil "* Move~%move body~%"))
           (target-text "")
           (source (parse-org source-text))
           (target (parse-org target-text
                              :limits (make-limits :max-output-length 8)))
           (condition
             (edit-relocate-test-limit-error
              (lambda ()
                (refile-subtree source
                                (edit-relocate-test-headline source "Move")
                                target nil)))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when condition
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-output-length))
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text))))

  (it "enforces the destination heading-depth limit before returning an edit"
    (let* ((source-text (format nil "* Move~%"))
           (target-text (format nil "* Parent~%"))
           (source (parse-org source-text))
           (target (parse-org target-text
                              :limits (make-limits :max-heading-depth 1)))
           (condition
             (edit-relocate-test-limit-error
              (lambda ()
                (refile-subtree source
                                (edit-relocate-test-headline source "Move")
                                target
                                (edit-relocate-test-headline target "Parent"))))))
      (with-soft-assertions
        (expect (typep condition 'org-resource-limit-error) :to-be t)
        (when condition
          (expect (org-resource-limit-error-limit-name condition)
                  :to-be :max-heading-depth))
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text))))

  (it "copies inherited archive tags and writes deterministic archive context"
    (let* ((fixture (edit-relocate-test-fixture
                     "t/fixtures/e/relocate/archive-context.sexp"))
           (policy (edit-relocate-test-fixture
                    "t/fixtures/e/policy/archive/archive-time-omitted.sexp"))
           (source-text (getf fixture :source-input))
           (target-text (getf fixture :target-input))
           (source (parse-org source-text :source-name (getf fixture :source-file)))
           (target (parse-org target-text))
           (heading
             (edit-relocate-test-headline source (getf fixture :source-heading)))
           (archive-heading
             (edit-relocate-test-headline target (getf fixture :target-heading)))
           (result (archive-subtree-to source heading target
                                       :target-heading archive-heading))
           (source-edit (relocation-result-source-edit result))
           (target-edit (relocation-result-target-edit result))
           (updated-source (edit-result-updated-document source-edit))
           (updated-target (edit-result-updated-document target-edit))
           (updated-text (document-text updated-target)))
      (with-soft-assertions
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text)
        (expect (document-text updated-source)
                :to-equal (getf fixture :source-expected))
        (expect updated-text :to-equal (getf fixture :target-expected))
        (expect (search ":ARCHIVE_CATEGORY: projects" updated-text) :to-be-truthy)
        (expect (search ":ARCHIVE_FILE: tasks.org" updated-text) :to-be-truthy)
        (expect (search ":ARCHIVE_OLPATH: Parent/Child" updated-text) :to-be-truthy)
        (expect (search ":ARCHIVE_TODO: TODO" updated-text) :to-be-truthy)
        (expect (search ":ARCHIVE_ITAGS: FILE PARENT CHILD" updated-text)
                :to-be-truthy)
        (expect (node-property
                 (edit-relocate-test-headline updated-target "Work") :tags)
                :to-equal '("FILE" "PARENT" "CHILD" "LOCAL"))
        (expect (getf policy :divergence)
                :to-be :archive-time-omitted-no-time-context)
        (expect (getf policy :additional-divergence)
                :to-be :org-archive-file-header-omitted-for-caller-target)
        (expect (search ":ARCHIVE_TIME:" (getf policy :oracle-target-expected))
                :to-be-truthy)
        (expect (search ":ARCHIVE_TIME:" updated-text) :to-be nil)
        (expect (search "Archived entries from file" updated-text)
                :to-be nil)
        (expect (relocation-result-mode result) :to-be :move))))

  (it "writes archive outliner paths without CR from CRLF headline values"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source-text (concatenate 'string "* Parent" crlf "** Child" crlf
                                     "*** Work" crlf "body" crlf))
           (source (parse-org source-text :source-name "tasks.org"))
           (target (parse-org (format nil "* Archive~%")))
           (heading (edit-relocate-test-headline source "Work"))
           (archive-heading
             (edit-relocate-test-headline target "Archive"))
           (result (archive-subtree-to source heading target
                                       :target-heading archive-heading))
           (updated-text
             (document-text
              (edit-result-updated-document
               (relocation-result-target-edit result)))))
      (with-soft-assertions
        (expect (search ":ARCHIVE_OLPATH: Parent/Child" updated-text)
                :to-be-truthy)
        (expect (search (concatenate 'string ":ARCHIVE_OLPATH: Parent"
                                     (string #\Return) "/Child")
                        updated-text)
                :to-be nil))))

  (it "matches archive metadata from every Org archive oracle fixture"
    (dolist (relative-path
              '("t/fixtures/e/oracle/archive/archive-root.sexp"
                "t/fixtures/e/oracle/archive/archive-heading.sexp"
                "t/fixtures/e/oracle/archive/archive-context.sexp"))
      (let* ((fixture (edit-relocate-test-fixture relative-path))
             (source-text (getf fixture :source-input))
             (source (parse-org source-text
                                :source-name (getf fixture :source-file)))
             (archive-location (getf fixture :archive-location))
             (archive-heading-p (search "::* Archive" archive-location))
             (target-text (if archive-heading-p
                              (format nil "* Archive~%")
                              (getf fixture :target-input)))
             (target (parse-org target-text))
             (heading (edit-relocate-test-headline source "Work"))
             (target-heading (and archive-heading-p
                                  (edit-relocate-test-headline target "Archive")))
             (result (archive-subtree-to source heading target
                                         :target-heading target-heading))
             (updated-source
               (edit-result-updated-document
                (relocation-result-source-edit result)))
             (updated-target
               (edit-result-updated-document
                (relocation-result-target-edit result)))
             (updated-text (document-text updated-target))
             (expected-text (getf fixture :target-expected)))
        (with-soft-assertions
          (expect (getf fixture :org-version) :to-equal "9.8.10")
          (expect (document-text source) :to-equal source-text)
          (expect (document-text target) :to-equal target-text)
          (expect (document-text updated-source)
                  :to-equal (getf fixture :source-expected))
          (expect (edit-relocate-test-property-value expected-text
                                                     "ARCHIVE_TIME")
                  :to-equal (getf fixture :archive-time))
          (expect (edit-relocate-test-property-value updated-text
                                                     "ARCHIVE_TIME")
                  :to-be nil)
          (dolist (property '("ARCHIVE_FILE" "ARCHIVE_OLPATH"
                              "ARCHIVE_CATEGORY" "ARCHIVE_TODO"
                              "ARCHIVE_ITAGS"))
            (expect (edit-relocate-test-property-value updated-text property)
                    :to-equal
                    (edit-relocate-test-property-value expected-text property)))
          (expect (node-property
                   (edit-relocate-test-headline updated-target "Work") :level)
                  :to-be (if archive-heading-p 2 1))))))

  (it "inserts archive properties into a CRLF property drawer"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source-text
             (format nil "* Task~%:PROPERTIES:~%:CUSTOM: keep~%:END:~%body~%"))
           (source-text
             (with-output-to-string (stream)
               (loop for character across source-text
                     do (if (char= character #\Newline)
                            (write-string crlf stream)
                            (write-char character stream)))))
           (source (parse-org source-text :source-name "tasks.org"))
           (target (parse-org ""))
           (result
             (archive-subtree-to
              source (edit-relocate-test-headline source "Task") target))
           (updated (edit-result-updated-document
                     (relocation-result-target-edit result)))
           (updated-text (document-text updated)))
      (with-soft-assertions
        (expect (document-text source) :to-equal source-text)
        (expect updated-text
                :to-equal
                (format nil
                        "* Task~C~C:PROPERTIES:~C~C:CUSTOM: keep~C~C:ARCHIVE_FILE: tasks.org~C~C:ARCHIVE_CATEGORY: tasks~C~C:END:~C~Cbody~C~C"
                        #\Return #\Newline #\Return #\Newline
                        #\Return #\Newline #\Return #\Newline
                        #\Return #\Newline #\Return #\Newline
                        #\Return #\Newline)))))

  (it "uses the target line ending for refiled archive content and properties"
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source-text (format nil "* Work~Abody~A" crlf crlf))
           (source (parse-org source-text :source-name "tasks.org"))
           (target-text (format nil "* Archive~%"))
           (target (parse-org target-text))
           (result
             (archive-subtree-to
              source (edit-relocate-test-headline source "Work") target
              :target-heading (edit-relocate-test-headline target "Archive")))
           (updated-source (edit-result-updated-document
                            (relocation-result-source-edit result)))
           (updated-target (edit-result-updated-document
                            (relocation-result-target-edit result))))
      (with-soft-assertions
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text)
        (expect (document-text updated-source) :to-equal "")
        (expect (document-text updated-target)
                :to-equal
                (format nil
                        "* Archive~%** Work~%:PROPERTIES:~%:ARCHIVE_FILE: tasks.org~%:ARCHIVE_CATEGORY: tasks~%:END:~%body~%")))))

  (it "adds and removes the local ARCHIVE tag while preserving other tags"
    (let* ((source-text (format nil "* Task :LOCAL:~%"))
           (document (parse-org source-text))
           (heading (edit-relocate-test-headline document "Task"))
           (added (edit-result-updated-document
                   (set-heading-archive-tag document heading t)))
           (removed (edit-result-updated-document
                     (set-heading-archive-tag
                      added (edit-relocate-test-headline added "Task") nil))))
      (with-soft-assertions
        (expect (document-text document) :to-equal source-text)
        (expect (document-text added) :to-equal (format nil "* Task :LOCAL:ARCHIVE:~%"))
        (expect (node-property (edit-relocate-test-headline added "Task") :tags)
                :to-equal '("LOCAL" "ARCHIVE"))
        (expect (document-text removed) :to-equal source-text))))

  (it "rejects a move direction without an adjacent sibling"
    (let* ((source-text (format nil "* Only~%"))
           (document (parse-org source-text))
           (condition
             (edit-relocate-test-edit-error
              (lambda ()
                (move-subtree document
                              (edit-relocate-test-headline document "Only")
                              :direction :down))))
           (invalid-direction
             (edit-relocate-test-edit-error
              (lambda ()
                (move-subtree document
                              (edit-relocate-test-headline document "Only")
                              :direction :sideways)))))
      (with-soft-assertions
        (expect (typep condition 'org-edit-error) :to-be t)
        (when condition
          (expect (org-edit-error-reason condition) :to-be :invalid-value))
        (expect (org-edit-error-reason invalid-direction)
                :to-be :invalid-value)
        (expect (document-text document) :to-equal source-text))))

  (it "moves a child upward without changing the other sibling order"
    (let* ((source-text (format nil "* Parent~%** A~%** B~%** C~%"))
           (document (parse-org source-text))
           (result (move-subtree document
                                 (edit-relocate-test-headline document "B")
                                 :direction :up))
           (updated (edit-result-updated-document result)))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal (format nil "* Parent~%** B~%** A~%** C~%"))
        (expect (document-text document) :to-equal source-text))))

  (it "rejects invalid relocation inputs at the public entry points"
    (let* ((source-text (format nil "* Source~%"))
           (source (parse-org source-text))
           (target (parse-org ""))
           (heading (edit-relocate-test-headline source "Source"))
           (mode-error
             (edit-relocate-test-edit-error
              (lambda ()
                (refile-subtree source heading target nil :mode :link))))
           (document-error
             (handler-case
                 (move-subtree :not-a-document nil :direction :down)
               (type-error () :type-error))))
      (with-soft-assertions
        (expect (org-edit-error-reason mode-error) :to-be :invalid-value)
        (expect document-error :to-be :type-error)
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal ""))))

  (it "refiles from a later parent to an earlier nested target"
    (let* ((source-text
             (format nil "* Earlier~%** Target~%* Later~%** Source~%"))
           (document (parse-org source-text))
           (result
             (refile-subtree document
                             (edit-relocate-test-headline document "Source")
                             document
                             (edit-relocate-test-headline document "Target")))
           (updated (edit-result-updated-document
                     (relocation-result-target-edit result))))
      (with-soft-assertions
        (expect (document-text updated)
                :to-equal (format nil
                                  "* Earlier~%** Target~%*** Source~%* Later~%"))
        (expect (relocation-result-target-path result) :to-be-truthy)
        (expect (document-text document) :to-equal source-text))))

  (it "rejects trailing comments and affiliated keywords at destination boundaries"
    (let* ((source-text (format nil "* Source~%"))
           (source (parse-org source-text))
           (source-heading (edit-relocate-test-headline source "Source"))
           (comment-text (format nil "* Target~%# trailing comment"))
           (comment-document (parse-org comment-text))
           (comment-error
             (edit-relocate-test-edit-error
              (lambda ()
                (refile-subtree source source-heading comment-document
                                (edit-relocate-test-headline comment-document
                                                             "Target")))))
           (comment-keyword-text
             (format nil "* Target~%#+COMMENT:"))
           (comment-keyword-document (parse-org comment-keyword-text))
           (comment-keyword-error
             (edit-relocate-test-edit-error
              (lambda ()
                (refile-subtree
                 source source-heading comment-keyword-document
                 (edit-relocate-test-headline comment-keyword-document
                                              "Target")))))
           (heading-affiliation-text
             (format nil "* Target~%~%#+CAPTION: trailing~%"))
           (heading-affiliation-document (parse-org heading-affiliation-text))
           (heading-affiliation-error
             (edit-relocate-test-edit-error
              (lambda ()
                (refile-subtree
                 source source-heading heading-affiliation-document
                 (edit-relocate-test-headline heading-affiliation-document
                                              "Target")))))
           (crlf (format nil "~C~C" #\Return #\Newline))
           (affiliation-text
             (concatenate 'string "#+CAPTION: trailing" crlf crlf))
           (affiliation-document (parse-org affiliation-text))
           (affiliation-error
             (edit-relocate-test-edit-error
              (lambda ()
                (refile-subtree source source-heading affiliation-document nil)))))
      (with-soft-assertions
        (expect (org-edit-error-reason comment-error)
                :to-be :uneditable-structure)
        (expect (org-edit-error-reason comment-keyword-error)
                :to-be :uneditable-structure)
        (expect (org-edit-error-reason heading-affiliation-error)
                :to-be :uneditable-structure)
        (expect (org-edit-error-reason affiliation-error)
                :to-be :uneditable-structure)
        (expect (document-text source) :to-equal source-text)
        (expect (document-text comment-document) :to-equal comment-text)
        (expect (document-text comment-keyword-document)
                :to-equal comment-keyword-text)
        (expect (document-text heading-affiliation-document)
                :to-equal heading-affiliation-text)
        (expect (document-text affiliation-document) :to-equal affiliation-text))))

  (it "inserts archive context after planning and keeps nested headings"
    (let* ((source-text
             (format nil "* Task~%SCHEDULED: <2025-01-01 Wed>~%~%body~%** Nested~%"))
           (source (parse-org source-text :source-name "tasks.org"))
           (target (parse-org ""))
           (result
             (archive-subtree-to source
                                 (edit-relocate-test-headline source "Task")
                                 target))
           (updated-target
             (edit-result-updated-document
              (relocation-result-target-edit result)))
           (updated-text (document-text updated-target)))
      (with-soft-assertions
        (expect (search "SCHEDULED: <2025-01-01 Wed>" updated-text) :to-be-truthy)
        (expect (search ":ARCHIVE_FILE: tasks.org" updated-text) :to-be-truthy)
        (expect (search "** Nested" updated-text) :to-be-truthy)
        (expect (document-text source) :to-equal source-text))))

  (it "inserts archive metadata into a no-newline subtree"
    (let* ((source-text "* Task")
           (source (parse-org source-text :source-name "tasks.org"))
           (target-text (format nil "* Archive~%"))
           (target (parse-org target-text))
           (result
             (archive-subtree-to
              source (edit-relocate-test-headline source "Task") target
              :target-heading (edit-relocate-test-headline target "Archive")))
           (updated-target
             (edit-result-updated-document
              (relocation-result-target-edit result)))
           (updated-text (document-text updated-target)))
      (with-soft-assertions
        (expect (search ":ARCHIVE_FILE: tasks.org" updated-text) :to-be-truthy)
        (expect (search (format nil "~%:PROPERTIES:~%") updated-text)
                :to-be-truthy)
        (expect (document-text source) :to-equal source-text)
        (expect (document-text target) :to-equal target-text))))

  (it "archives a plain subtree without inventing archive metadata"
    (let* ((source-text (format nil "* Plain~%"))
           (source (parse-org source-text))
           (target (parse-org ""))
           (result
             (archive-subtree-to source
                                 (edit-relocate-test-headline source "Plain")
                                 target))
           (updated-source
             (edit-result-updated-document
              (relocation-result-source-edit result)))
           (updated-target
             (edit-result-updated-document
              (relocation-result-target-edit result))))
      (with-soft-assertions
        (expect (document-text updated-source) :to-equal "")
        (expect (document-text updated-target) :to-equal source-text)
        (expect (search ":PROPERTIES:" (document-text updated-target)) :to-be nil)
        (expect (document-text source) :to-equal source-text))))

  (it "updates archive metadata already present in a property drawer"
    (let* ((source-text
             (format nil "* Task~%:PROPERTIES:~%:ARCHIVE_FILE: old.org~%:ARCHIVE_CATEGORY: old~%:END:~%"))
           (source (parse-org source-text :source-name "tasks.org"))
           (target (parse-org ""))
           (result
             (archive-subtree-to source
                                 (edit-relocate-test-headline source "Task")
                                 target))
           (updated-target
             (edit-result-updated-document
              (relocation-result-target-edit result)))
           (updated-text (document-text updated-target)))
      (with-soft-assertions
        (expect (search ":ARCHIVE_FILE: tasks.org" updated-text) :to-be-truthy)
        (expect (search ":ARCHIVE_CATEGORY: tasks" updated-text) :to-be-truthy)
        (expect (search "old.org" updated-text) :to-be nil)
        (expect (document-text source) :to-equal source-text))))

  (it "rejects duplicated archive metadata and ambiguous drawers"
    (let* ((duplicate-text
             (format nil "* Task~%:PROPERTIES:~%:ARCHIVE_FILE: old~%:ARCHIVE_FILE: older~%:END:~%"))
           (duplicate-source
             (parse-org duplicate-text :source-name "tasks.org"))
           (duplicate-target (parse-org ""))
           (duplicate-error
             (edit-relocate-test-edit-error
              (lambda ()
                (archive-subtree-to
                 duplicate-source
                 (edit-relocate-test-headline duplicate-source "Task")
                 duplicate-target))))
           (multiple-text
             (format nil "* Task~%:PROPERTIES:~%:CUSTOM: one~%:END:~%DEADLINE: <2025-01-01 Wed>~%:PROPERTIES:~%:CUSTOM: two~%:END:~%"))
           (multiple-source
             (parse-org multiple-text :source-name "tasks.org"))
           (multiple-target (parse-org ""))
           (multiple-error
             (edit-relocate-test-edit-error
              (lambda ()
                (archive-subtree-to
                 multiple-source
                 (edit-relocate-test-headline multiple-source "Task")
                 multiple-target))))
           (unclosed-text
             (format nil "* Task~%:PROPERTIES:~%:CUSTOM: keep~%"))
           (unclosed-source
             (parse-org unclosed-text :source-name "tasks.org"))
           (unclosed-target (parse-org ""))
           (unclosed-error
             (edit-relocate-test-edit-error
              (lambda ()
                (archive-subtree-to
                 unclosed-source
                 (edit-relocate-test-headline unclosed-source "Task")
                 unclosed-target)))))
      (with-soft-assertions
        (expect (org-edit-error-reason duplicate-error)
                :to-be :uneditable-structure)
        (expect (org-kit-error-message duplicate-error)
                :to-equal "Archive metadata property is duplicated")
        (expect (org-edit-error-reason multiple-error)
                :to-be :uneditable-structure)
        (expect (org-kit-error-message multiple-error)
                :to-equal "Headline has multiple property drawers")
        (expect (org-edit-error-reason unclosed-error)
                :to-be :uneditable-structure)
        (expect (org-kit-error-message unclosed-error)
                :to-equal "Property drawer has no closing marker")
        (expect (document-text duplicate-source) :to-equal duplicate-text)
        (expect (document-text duplicate-target) :to-equal "")
        (expect (document-text multiple-source) :to-equal multiple-text)
        (expect (document-text multiple-target) :to-equal "")
        (expect (document-text unclosed-source) :to-equal unclosed-text)
        (expect (document-text unclosed-target) :to-equal ""))))

  (it "preserves an existing archive tag when setting it again"
    (let* ((source-text (format nil "* Task :ARCHIVE:~%"))
           (document (parse-org source-text))
           (result
             (set-heading-archive-tag
              document (edit-relocate-test-headline document "Task") t)))
      (with-soft-assertions
        (expect (document-text (edit-result-updated-document result))
                :to-equal source-text)
        (expect (document-text document) :to-equal source-text)))))
