;;;; t/todo-settings-test.lisp
(in-package #:cl-org-kit/test)

(defun todo-settings-test-source (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun todo-settings-test-headlines (document)
  (let ((headlines nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(describe "TODO and document setting normalization"
  (it "preserves sequence metadata while flattening only the active group"
    (let* ((document
             (parse-org
              (todo-settings-test-source
               "#+TODO[FlowId]: TODO(t) WAIT(w@/!) | DONE(d!)"
               "#+SEQ_TODO: NEXT(n) HOLD(h) | FINISHED(f)"
               "#+TYP_TODO[TypeId]: IDEA(i!) | REJECTED(r@/)"
               "* TODO First"
               "* WAIT Waiting"
               "* DONE Complete"
               "* NEXT Next"
               "* FINISHED Finished"
               "* IDEA Idea"
               "* REJECTED Rejected")))
           (settings (document-settings document))
           (headlines (todo-settings-test-headlines document)))
      (with-soft-assertions
        (expect (getf settings :todo-keywords)
                :to-equal '(("IDEA" . :todo) ("REJECTED" . :done)))
        (expect (getf settings :todo-sequences)
                :to-equal
                '((:kind :sequence
                   :declaration-kind :todo
                   :sequence-id "FlowId"
                   :declaration-index 1
                   :todo-keywords
                   ((:keyword "TODO" :logging-mode :none
                     :exit-logging-p nil :fast-key #\t)
                    (:keyword "WAIT" :logging-mode :note
                     :exit-logging-p t :fast-key #\w))
                   :done-keywords
                   ((:keyword "DONE" :logging-mode :timestamp
                     :exit-logging-p nil :fast-key #\d)))
                  (:kind :sequence
                   :declaration-kind :seq-todo
                   :sequence-id "seq-todo-2"
                   :declaration-index 2
                   :todo-keywords
                   ((:keyword "NEXT" :logging-mode :none
                     :exit-logging-p nil :fast-key #\n)
                    (:keyword "HOLD" :logging-mode :none
                     :exit-logging-p nil :fast-key #\h))
                   :done-keywords
                   ((:keyword "FINISHED" :logging-mode :none
                     :exit-logging-p nil :fast-key #\f)))
                  (:kind :type
                   :declaration-kind :typ-todo
                   :sequence-id "TypeId"
                   :declaration-index 3
                   :todo-keywords
                   ((:keyword "IDEA" :logging-mode :timestamp
                     :exit-logging-p nil :fast-key #\i))
                   :done-keywords
                   ((:keyword "REJECTED" :logging-mode :note
                     :exit-logging-p t :fast-key #\r)))))
        (expect (mapcar (lambda (headline)
                          (list (node-property headline :todo-keyword)
                                (node-property headline :todo-type)))
                        headlines)
                :to-equal
                '(("TODO" :todo) ("WAIT" :todo) ("DONE" :done)
                  ("NEXT" :todo) ("FINISHED" :done)
                  ("IDEA" :todo) ("REJECTED" :done))))))

  (it "uses active keywords first when an inactive declaration repeats one"
    (let* ((document
             (parse-org
              (todo-settings-test-source
               "#+TODO: SHARED | OLD-DONE"
               "#+TYP_TODO: OLD-DONE | SHARED"
               "* SHARED State")))
           (headline (first (todo-settings-test-headlines document))))
      (with-soft-assertions
        (expect (getf (document-settings document) :todo-keywords)
                :to-equal '(("OLD-DONE" . :todo) ("SHARED" . :done)))
        (expect (node-property headline :todo-type) :to-be :done))))

  (it "keeps multiple active TYP_TODO declarations in source order"
    (let* ((document
             (parse-org
              (todo-settings-test-source
               "#+TYP_TODO[first]: IDEA | REJECTED"
               "#+TYP_TODO[second]: BUG | FIXED"
               "* IDEA One"
               "* BUG Two")))
           (settings (document-settings document))
           (sequences (getf settings :todo-sequences)))
      (with-soft-assertions
        (expect (mapcar (lambda (sequence)
                          (list (getf sequence :kind)
                                (getf sequence :sequence-id)
                                (getf sequence :declaration-index)))
                        sequences)
                :to-equal '((:type "first" 1) (:type "second" 2)))
        (expect (getf settings :todo-keywords)
                :to-equal '(("IDEA" . :todo) ("REJECTED" . :done)
                            ("BUG" . :todo) ("FIXED" . :done))))))

  (it "rejects duplicate keywords in active and ambiguous inactive groups"
    (with-soft-assertions
      (signals org-parse-error
               (parse-org
                (todo-settings-test-source
                 "#+TODO: FIRST | SHARED"
                 "#+SEQ_TODO: SHARED | LAST")))
      (signals org-parse-error
               (parse-org
                (todo-settings-test-source
                 "#+TODO: FIRST | DONE"
                 "#+SEQ_TODO: SECOND | DONE")))
      (signals org-parse-error
               (parse-org
                (todo-settings-test-source
                 "#+TODO: SAME | OLD-DONE"
                 "#+SEQ_TODO: SAME | OTHER-DONE"
                 "#+TYP_TODO: TYPE | TYPE-DONE")))))

  (it "rejects ambiguous IDs, empty keyword names, and malformed TODO keys"
    (with-soft-assertions
      (signals org-parse-error
               (parse-org
                (todo-settings-test-source
                 "#+TODO[flow]: FIRST | LAST"
                 "#+SEQ_TODO[flow]: NEXT | FINISHED")))
      (signals org-parse-error
               (parse-org "#+TODO: TODO | TODO"))
      (signals org-parse-error
               (parse-org "#+TODO: (TODO) DONE"))
      (signals org-parse-error
               (parse-org "#+SEQ_TODO[flow: NEXT | DONE"))))

  (it "signals a located parse error and preserves success and error inputs"
    (let* ((valid-input
             (copy-seq
              (todo-settings-test-source
               "#+TODO: OPEN | CLOSED"
               "* OPEN Item")))
           (valid-snapshot (copy-seq valid-input))
           (invalid-input (copy-seq "#+TODO: (TODO) DONE"))
           (invalid-snapshot (copy-seq invalid-input))
           (valid-document (parse-org valid-input :source-name "valid.org"))
           (condition nil))
      (setf condition
            (handler-case
                (progn
                  (parse-org invalid-input :source-name "invalid.org")
                  nil)
              (org-parse-error (caught) caught)))
      (with-soft-assertions
        (expect (document-text valid-document) :to-equal valid-snapshot)
        (expect (string= valid-input valid-snapshot) :to-be t)
        (expect (string= invalid-input invalid-snapshot) :to-be t)
        (expect (typep condition 'org-parse-error) :to-be t)
        (when condition
          (expect (org-kit-error-message condition)
                  :to-equal "Invalid or empty TODO keyword in (TODO)")
          (expect (org-parse-error-offset condition) :to-be 0)
          (expect (org-parse-error-line condition) :to-be 1)
          (expect (org-parse-error-column condition) :to-be 1)
          (expect (org-parse-error-context condition)
                  :to-equal "#+TODO: (TODO) DONE")
          (expect (org-parse-error-source-name condition)
                  :to-equal "invalid.org")))))

  (it "returns a parse error from the legacy one-argument sequence helper"
    (signals org-parse-error
             (org-kit::reader-heading-todo-sequence "TODO()")))

  (it "normalizes FILETAGS, TAGS, and PROPERTY source entries"
    (let ((settings
            (document-settings
             (parse-org
              (todo-settings-test-source
               "#+FILETAGS: :work:project:"
               "#+TAGS: :work:team:"
               "#+TAGS: @home(h) @work(w)"
               "#+PROPERTY: header-args :eval never"
               "#+PROPERTY: CATEGORY Notes")))))
      (with-soft-assertions
        (expect (getf settings :file-tags)
                :to-equal '("work" "project"))
        (expect (getf settings :tag-alist)
                :to-equal '((:tag "work" :fast-key nil)
                            (:tag "team" :fast-key nil)
                            (:tag "@home" :fast-key #\h)
                            (:tag "@work" :fast-key #\w)))
        (expect (getf settings :property-settings)
                :to-equal '(("HEADER-ARGS" . ":eval never")
                            ("CATEGORY" . "Notes"))))))

  (it "omits tag and property settings when no corresponding directive exists"
    (let* ((document (parse-org "* Plain heading"))
           (settings (document-settings document)))
      (with-soft-assertions
        (expect (member :file-tags settings) :to-be nil)
        (expect (member :tag-alist settings) :to-be nil)
        (expect (member :property-settings settings) :to-be nil)
        (expect (org-kit::org-setting-value document "FILETAGS" :fallback)
                :to-equal :fallback))))

  (it "ignores fake directives inside source and example blocks"
    (let* ((document
             (parse-org
              (todo-settings-test-source
               "#+TODO: OPEN | CLOSED"
               "#+BEGIN_SRC org"
               "#+TODO: (INVALID) BROKEN"
               "#+FILETAGS: invalid"
               "#+END_SRC"
               "#+BEGIN_EXAMPLE"
               "#+SEQ_TODO[unterminated: NEXT | DONE"
               "#+END_EXAMPLE"
               "* OPEN Task")))
           (settings (document-settings document))
           (headline (first (todo-settings-test-headlines document))))
      (with-soft-assertions
        (expect (getf settings :todo-keywords)
                :to-equal '(("OPEN" . :todo) ("CLOSED" . :done)))
        (expect (getf settings :file-tags) :to-equal nil)
        (expect (node-property headline :todo-keyword) :to-equal "OPEN"))))

  (it "synthesizes TODO and DONE only when no declaration exists"
    (let* ((settings (document-settings (parse-org "* TODO Default")))
           (sequence (first (getf settings :todo-sequences))))
      (with-soft-assertions
        (expect (getf settings :todo-keywords)
                :to-equal '(("TODO" . :todo) ("DONE" . :done)))
        (expect (list (getf sequence :kind)
                      (getf sequence :declaration-kind)
                      (getf sequence :sequence-id)
                      (getf sequence :declaration-index))
                :to-equal '(:sequence :todo "todo-1" 1))
        (expect (getf sequence :done-keywords)
                :to-equal
                '((:keyword "DONE" :logging-mode :none
                   :exit-logging-p nil :fast-key nil)))))))
