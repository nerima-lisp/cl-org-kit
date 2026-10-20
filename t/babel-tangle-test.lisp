;;;; t/babel-tangle-test.lisp
(in-package #:cl-org-kit/test)

(defun babel-tangle-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun babel-tangle-test-blocks (document)
  (let ((blocks nil))
    (map-nodes
     (lambda (node)
       (when (eq (node-type node) :src-block)
         (push node blocks)))
     (document-root document))
    (nreverse blocks)))

(defun babel-tangle-test-entry (plan path)
  (find path (coerce (write-plan-entries plan) 'list)
        :key #'write-plan-entry-path :test #'string=))

(defun babel-tangle-test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (error (condition) condition)))

(defun babel-tangle-test-document (text &key source-name limits)
  (parse-org text :source-name source-name
                  :limits (or limits (make-limits))))

(describe "Babel tangling"
  (it "groups blocks in document order and returns a plan without writing"
    (let* ((source-name
             "babel-tangle-no-write.org")
           (target-name
             "babel-tangle-no-write.el")
           (document
             (babel-tangle-test-document
              (babel-tangle-test-lines
               "#+BEGIN_SRC emacs-lisp :tangle yes"
               "    (first)"
               "#+END_SRC"
               ""
               "#+BEGIN_SRC emacs-lisp :tangle yes"
               "    (second)"
               "#+END_SRC"
               ""
               "#+BEGIN_SRC emacs-lisp :tangle yes :padline no"
               "    (third)"
               "#+END_SRC")
              :source-name source-name))
           (plan (babel-tangle document)))
      (with-soft-assertions
        (expect (write-plan-p plan) :to-be t)
        (expect (length (write-plan-entries plan)) :to-be 1)
        (expect (write-plan-entry-path (aref (write-plan-entries plan) 0))
                :to-equal target-name)
        (expect (write-plan-entry-content (aref (write-plan-entries plan) 0))
                :to-equal (babel-tangle-test-lines
                           "(first)" "" "(second)" "(third)" ""))
        (expect (probe-file target-name) :to-be nil))))

  (it "uses target-file as a default while honoring explicit tangle headers"
    (let* ((document
             (babel-tangle-test-document
              (babel-tangle-test-lines
               "#+BEGIN_SRC emacs-lisp"
               "default body"
               "#+END_SRC"
               ""
               "#+BEGIN_SRC emacs-lisp :tangle no"
               "skipped body"
               "#+END_SRC"
               ""
               "#+BEGIN_SRC emacs-lisp :tangle custom.lisp"
               "custom body"
               "#+END_SRC"
               ""
               "#+BEGIN_SRC emacs-lisp :tangle yes"
               "derived body"
               "#+END_SRC"
               ""
               "* Scoped"
               ":PROPERTIES:"
               ":header-args:emacs-lisp: :tangle inherited.el"
               ":END:"
               "#+BEGIN_SRC emacs-lisp"
               "inherited body"
               "#+END_SRC")
              :source-name "guide.org"))
           (plan (babel-tangle document :target-file "default.txt")))
      (with-soft-assertions
        (expect (map 'list #'write-plan-entry-path (write-plan-entries plan))
                :to-equal '("default.txt" "custom.lisp" "guide.el" "inherited.el"))
        (expect (write-plan-entry-content
                 (babel-tangle-test-entry plan "default.txt"))
                :to-equal (format nil "default body~%"))
        (expect (write-plan-entry-content
                 (babel-tangle-test-entry plan "custom.lisp"))
                :to-equal (format nil "custom body~%"))
        (expect (write-plan-entry-content
                 (babel-tangle-test-entry plan "inherited.el"))
                :to-equal (format nil "inherited body~%")))))

  (it "filters languages and preserves Org tangle body indentation"
    (let* ((document
             (babel-tangle-test-document
              (babel-tangle-test-lines
               "#+BEGIN_SRC emacs-lisp"
               "  (kept)"
               "#+END_SRC"
               ""
               "#+BEGIN_SRC shell"
               "    skipped"
               "#+END_SRC"
               ""
               "#+BEGIN_SRC text"
               "no filter match"
               "#+END_SRC")))
           (plan (babel-tangle document :target-file "filtered.txt"
                               :language-regexp "^(emacs-lisp|shell)$"))
           (trim-document
             (babel-tangle-test-document
              (babel-tangle-test-lines
               "#+BEGIN_SRC text :tangle trimmed.txt"
               "    "
               "  first"
               "  second   "
               "    "
               "#+END_SRC")))
           (trim-entry (babel-tangle-test-entry
                        (babel-tangle trim-document) "trimmed.txt")))
      (with-soft-assertions
        (expect (length (write-plan-entries plan)) :to-be 1)
        (expect (write-plan-entry-content
                 (aref (write-plan-entries plan) 0))
                :to-equal (format nil "(kept)~%~%skipped~%"))
        (expect (write-plan-entry-content trim-entry)
                :to-equal (format nil "first~%second~%")))))

  (it "adds shebang, mode, and directory creation to the plan"
    (let* ((parent-directory "babel-tangle-no-write-parent")
           (parent-existed-p (probe-file parent-directory))
           (document
             (babel-tangle-test-document
              (babel-tangle-test-lines
               "#+BEGIN_SRC shell :tangle babel-tangle-no-write-parent/nested/run.sh :mkdirp yes :shebang \"#!/bin/sh\" :padline no"
               "echo ready"
               "#+END_SRC")))
           (entry (aref (write-plan-entries (babel-tangle document)) 0))
           (special-document
             (babel-tangle-test-document
              (format nil "#+BEGIN_SRC shell :tangle special.sh :tangle-mode u+x~%body~%#+END_SRC")))
           (special-entry
             (aref (write-plan-entries (babel-tangle special-document)) 0)))
      (with-soft-assertions
        (expect (write-plan-entry-path entry)
                :to-equal "babel-tangle-no-write-parent/nested/run.sh")
        (expect (write-plan-entry-content entry)
                :to-equal (format nil "#!/bin/sh~%echo ready~%"))
        (expect (write-plan-entry-mode entry) :to-be #o755)
        (expect (write-plan-entry-create-directories-p entry) :to-be t)
        (expect parent-existed-p :to-be nil)
        (expect (probe-file parent-directory) :to-be nil)
        (expect (write-plan-entry-mode special-entry) :to-be #o744))))

  (it "renders Org comments and variable assignments through supplied callbacks"
    (let* ((document
             (babel-tangle-test-document
              (babel-tangle-test-lines
               "#+BEGIN_SRC shell :tangle vars.sh :comments both :var answer=42"
               "echo $answer"
               "#+END_SRC")))
           (comment-renderer
             (lambda (block kind target-path)
               (declare (ignore block target-path))
               (case kind
                 (:org "# source context")
                 (:link (values "# source link" "# source ends here")))))
           (variable-renderer
             (lambda (block variables)
               (declare (ignore block))
               (format nil "answer=~A" (cdar variables))))
           (entry
             (aref (write-plan-entries
                    (babel-tangle document
                                  :comment-renderer comment-renderer
                                  :variable-renderer variable-renderer))
                   0))
           (empty-comment-document
             (babel-tangle-test-document
              (format nil "#+BEGIN_SRC text :tangle empty-comments.txt :comments link~%body~%#+END_SRC")))
           (empty-comment-entry
             (aref (write-plan-entries
                    (babel-tangle
                     empty-comment-document
                     :comment-renderer
                     (lambda (block kind target-path)
                       (declare (ignore block kind target-path))
                       (values "" ""))))
                   0)))
      (with-soft-assertions
        (expect (write-plan-entry-content entry)
                :to-equal
                (format nil "# source context~%# source link~%answer=42~%echo $answer~%# source ends here~%"))
        (expect (write-plan-entry-content empty-comment-entry)
                :to-equal (format nil "body~%")))))

  (it "expands noweb references in the tangle context"
    (let* ((document
             (babel-tangle-test-document
              (babel-tangle-test-lines
               "#+NAME: fragment"
               "#+BEGIN_SRC text"
               "fragment"
               "#+END_SRC"
               ""
               "#+BEGIN_SRC text :tangle joined.txt :noweb tangle"
               "before"
               "<<fragment>>"
               "after"
               "#+END_SRC")))
           (entry (aref (write-plan-entries (babel-tangle document)) 0)))
      (expect (write-plan-entry-content entry)
              :to-equal (format nil "before~%fragment~%after~%"))))

  (it "rejects unsafe paths and invalid language filters"
    (flet ((assert-condition (condition type reader reason)
             (expect (typep condition type) :to-be t)
             (when (typep condition type)
               (expect (funcall reader condition) :to-be reason))))
      (let* ((default-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text~%body~%#+END_SRC")))
             (parent-path-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle a/../../escape.txt~%body~%#+END_SRC")))
             (absolute-path-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle /tmp/outside.txt~%body~%#+END_SRC")))
             (null-path-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle bad~Cpath.txt~%body~%#+END_SRC"
                        #\Null)))
             (self-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle same.org~%body~%#+END_SRC")
                :source-name "same.org"))
             (invalid-regexp-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle out.txt~%body~%#+END_SRC")))
             (parent-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle parent-path-document))))
             (absolute-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle absolute-path-document))))
             (null-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle null-path-document))))
             (self-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle self-document))))
             (regexp-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle invalid-regexp-document
                                         :language-regexp "["))))
             (target-parent-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle default-document
                                         :target-file "../outside.txt"))))
             (target-absolute-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle default-document
                                         :target-file "/tmp/outside.txt"))))
             (target-null-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle
                            default-document
                            :target-file (format nil "bad~Cpath.txt" #\Null))))))
        (with-soft-assertions
          (assert-condition parent-error 'org-write-plan-error
                            #'org-write-plan-error-reason :parent-component)
          (assert-condition absolute-error 'org-write-plan-error
                            #'org-write-plan-error-reason :absolute-path)
          (assert-condition null-error 'org-write-plan-error
                            #'org-write-plan-error-reason :invalid-path)
          (assert-condition self-error 'org-write-plan-error
                            #'org-write-plan-error-reason :same-source-file)
          (assert-condition regexp-error 'org-babel-error
                            #'org-babel-error-reason :invalid-language-regexp)
          (assert-condition target-parent-error 'org-write-plan-error
                            #'org-write-plan-error-reason :parent-component)
          (assert-condition target-absolute-error 'org-write-plan-error
                            #'org-write-plan-error-reason :absolute-path)
          (assert-condition target-null-error 'org-write-plan-error
                            #'org-write-plan-error-reason :invalid-path)))))

  (it "reports named renderer, mode, and resource-limit errors"
    (flet ((assert-condition (condition type reader reason)
             (expect (typep condition type) :to-be t)
             (when (typep condition type)
               (expect (funcall reader condition) :to-be reason))))
      (let* ((comments-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle comments.txt :comments link~%body~%#+END_SRC")))
             (variables-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle variables.txt :var x=1~%body~%#+END_SRC")))
             (mode-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle mode.txt :tangle-mode o1000~%body~%#+END_SRC")))
             (limited-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle limited.txt~%body~%#+END_SRC")
                :limits (make-limits :max-write-plan-bytes 4)))
             (output-length-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle length.txt~%body~%#+END_SRC")
                :limits (make-limits :max-output-length 4)))
             (output-octets-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle octets.txt~%body~%#+END_SRC")
                :limits (make-limits :max-output-octets 4)))
             (expansion-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle expanded.txt~%body~%#+END_SRC")
                :limits (make-limits :max-babel-expanded-characters 3)))
             (entry-limit-document
               (babel-tangle-test-document
                (babel-tangle-test-lines
                 "#+BEGIN_SRC text :tangle first.txt"
                 "one"
                 "#+END_SRC"
                 ""
                 "#+BEGIN_SRC text :tangle second.txt"
                 "two"
                 "#+END_SRC")
                :limits (make-limits :max-write-plan-entries 1)))
             (path-limit-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle nested/file.txt~%body~%#+END_SRC")
                :limits (make-limits :max-write-plan-path-depth 1)))
             (comments-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle comments-document))))
             (variables-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle variables-document))))
             (mode-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle mode-document))))
             (limit-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle limited-document))))
             (output-length-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle output-length-document))))
             (output-octets-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle output-octets-document))))
             (expansion-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle expansion-document))))
             (entry-limit-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle entry-limit-document))))
             (path-limit-error
               (babel-tangle-test-condition
                (lambda () (babel-tangle path-limit-document)))))
        (with-soft-assertions
          (assert-condition comments-error 'org-babel-error
                            #'org-babel-error-reason :missing-comment-renderer)
          (assert-condition variables-error 'org-babel-error
                            #'org-babel-error-reason :missing-variable-renderer)
          (assert-condition mode-error 'org-babel-error
                            #'org-babel-error-reason :invalid-tangle-mode)
          (assert-condition limit-error 'org-resource-limit-error
                            #'org-resource-limit-error-limit-name
                            :max-write-plan-bytes)
          (assert-condition output-length-error 'org-resource-limit-error
                            #'org-resource-limit-error-limit-name
                            :max-output-length)
          (assert-condition output-octets-error 'org-resource-limit-error
                            #'org-resource-limit-error-limit-name
                            :max-output-octets)
          (assert-condition expansion-error 'org-resource-limit-error
                            #'org-resource-limit-error-limit-name
                            :max-babel-expanded-characters)
          (assert-condition entry-limit-error 'org-resource-limit-error
                            #'org-resource-limit-error-limit-name
                            :max-write-plan-entries)
          (assert-condition path-limit-error 'org-resource-limit-error
                            #'org-resource-limit-error-limit-name
                            :max-write-plan-path-depth)))))

  (it "applies Org numeric and symbolic tangle modes"
    (with-soft-assertions
      (expect (org-kit::babel-tangle-mode "493") :to-be #o755)
      (expect (org-kit::babel-tangle-mode "rwxr-xr-x") :to-be #o755)
      (expect (org-kit::babel-tangle-mode "a=rw,u+x") :to-be #o766)
      (expect (org-kit::babel-tangle-mode "u+x,g-w") :to-be #o744)
      (expect (org-kit::babel-tangle-mode "o755") :to-be #o755)
      (expect (org-kit::babel-tangle-mode "o0755") :to-be #o755)
      (expect (org-kit::babel-tangle-mode "o777") :to-be #o777)
      (expect (org-kit::babel-tangle-mode #o777) :to-be #o777)
      (dolist (value '("755" "o000755" "o1000" "rwsr-sr-x"
                       "u+s" "g+s" "+t" "o+s" "u+t" "g+t" "o+t"
                       #o1000 #o4000 #o7777))
        (let ((condition
                (babel-tangle-test-condition
                 (lambda () (org-kit::babel-tangle-mode value)))))
          (expect (typep condition 'org-babel-error) :to-be t)
          (when (typep condition 'org-babel-error)
            (expect (org-babel-error-reason condition)
                    :to-be :invalid-tangle-mode))))))
  (it "validates tangle arguments and safely derives target paths"
    (let* ((source-text
             (format nil "#+BEGIN_SRC emacs-lisp :tangle yes~%body~%#+END_SRC"))
           (default-document (babel-tangle-test-document source-text))
           (missing-source-document
             (babel-tangle-test-document source-text :source-name nil))
           (pathname-document
             (babel-tangle-test-document
              source-text :source-name #p"guide/source.org"))
           (absolute-document
             (babel-tangle-test-document
              source-text :source-name "/tmp/source.org"))
           (unsafe-source-document
             (babel-tangle-test-document
              (format nil "#+BEGIN_SRC text :tangle out.txt~%body~%#+END_SRC")
              :source-name "../source.org"))
           (unparsed-document (make-document ""))
           (block (first (babel-tangle-test-blocks default-document)))
           (bad-document-error
             (babel-tangle-test-condition (lambda () (babel-tangle nil))))
           (unparsed-error
             (babel-tangle-test-condition
              (lambda () (babel-tangle unparsed-document))))
           (target-file-error
             (babel-tangle-test-condition
              (lambda () (babel-tangle default-document :target-file 7))))
           (regexp-type-error
             (babel-tangle-test-condition
              (lambda () (babel-tangle default-document :language-regexp 7))))
           (resolver-type-error
             (babel-tangle-test-condition
              (lambda () (babel-tangle default-document :result-resolver 7))))
           (comment-renderer-type-error
             (babel-tangle-test-condition
              (lambda () (babel-tangle default-document :comment-renderer 7))))
           (variable-renderer-type-error
             (babel-tangle-test-condition
              (lambda () (babel-tangle default-document :variable-renderer 7))))
           (missing-source-error
             (babel-tangle-test-condition
              (lambda () (babel-tangle missing-source-document))))
           (internal-destination-error
             (babel-tangle-test-condition
              (lambda ()
                (org-kit::babel-tangle-target-path
                 default-document block '(("TANGLE" . 7)) nil))))
           (pathname-plan (babel-tangle pathname-document))
           (absolute-plan (babel-tangle absolute-document))
           (unsafe-source-plan (babel-tangle unsafe-source-document))
           (target-file-plan
             (babel-tangle (babel-tangle-test-document
                            (format nil "#+BEGIN_SRC text~%body~%#+END_SRC"))
                           :target-file #p"default.txt"))
           (null-tangle-values
             (multiple-value-list
              (org-kit::babel-tangle-target-path
               default-document block '(("TANGLE" . nil)) nil))))
      (flet ((expect-reason (condition reason)
               (expect (typep condition 'org-babel-error) :to-be t)
               (when (typep condition 'org-babel-error)
                 (expect (org-babel-error-reason condition)
                         :to-be reason))))
        (with-soft-assertions
          (expect (typep bad-document-error 'type-error) :to-be t)
          (expect-reason unparsed-error :unparsed-document)
          (expect (typep target-file-error 'type-error) :to-be t)
          (expect (typep regexp-type-error 'type-error) :to-be t)
          (expect (typep resolver-type-error 'type-error) :to-be t)
          (expect (typep comment-renderer-type-error 'type-error) :to-be t)
          (expect (typep variable-renderer-type-error 'type-error) :to-be t)
          (expect-reason missing-source-error :missing-source-name)
          (expect (typep internal-destination-error 'type-error) :to-be t)
          (expect (map 'list #'write-plan-entry-path
                       (write-plan-entries pathname-plan))
                  :to-equal '("guide/source.el"))
          (expect (map 'list #'write-plan-entry-path
                       (write-plan-entries absolute-plan))
                  :to-equal '("source.el"))
          (expect (map 'list #'write-plan-entry-path
                       (write-plan-entries unsafe-source-plan))
                  :to-equal '("out.txt"))
          (expect (map 'list #'write-plan-entry-path
                       (write-plan-entries target-file-plan))
                  :to-equal '("default.txt"))
          (expect null-tangle-values :to-equal '(nil nil))))))

  (it "validates tangle renderers, symbolic permissions, and tab indentation"
    (let* ((org-comments-document
             (babel-tangle-test-document
              (format nil "#+BEGIN_SRC text :tangle comments.txt :comments org~%body~%#+END_SRC")))
           (unsupported-comments-document
             (babel-tangle-test-document
              (format nil "#+BEGIN_SRC text :tangle bad.txt :comments strange~%body~%#+END_SRC")))
           (link-comments-document
             (babel-tangle-test-document
              (format nil "#+BEGIN_SRC text :tangle link.txt :comments link~%body~%#+END_SRC")))
           (variables-document
             (babel-tangle-test-document
              (format nil "#+BEGIN_SRC text :tangle variables.txt :var answer=42~%body~%#+END_SRC")))
           (tab-document
             (babel-tangle-test-document
              (format nil "#+BEGIN_SRC text :tangle tabs.txt~%    first~%~Csecond~%#+END_SRC"
                      #\Tab)))
           (org-entry
             (aref (write-plan-entries
                    (babel-tangle
                     org-comments-document
                     :comment-renderer
                     (lambda (block kind target-path)
                       (declare (ignore block target-path))
                       (when (eq kind :org) "# org context"))))
                   0))
           (unsupported-comments-error
             (babel-tangle-test-condition
              (lambda () (babel-tangle unsupported-comments-document))))
           (invalid-comment-error
             (babel-tangle-test-condition
              (lambda ()
                (babel-tangle
                 org-comments-document
                 :comment-renderer
                 (lambda (block kind target-path)
                   (declare (ignore block kind target-path))
                   7)))))
           (invalid-link-error
             (babel-tangle-test-condition
              (lambda ()
                (babel-tangle
                 link-comments-document
                 :comment-renderer
                 (lambda (block kind target-path)
                   (declare (ignore block kind target-path))
                   (values "# link" nil))))))
           (invalid-variable-error
             (babel-tangle-test-condition
              (lambda ()
                (babel-tangle
                 variables-document
                 :variable-renderer
                 (lambda (block variables)
                   (declare (ignore block variables))
                   7)))))
           (tab-entry
             (aref (write-plan-entries (babel-tangle tab-document)) 0))
           (add-piece-error
             (babel-tangle-test-condition
              (lambda ()
                (org-kit::babel-tangle-add-piece
                 (org-kit::make-babel-tangle-budget :limits (make-limits))
                 (org-kit::make-babel-tangle-file :path "invalid.txt")
                 7))))
           (invalid-who
             (babel-tangle-test-condition
              (lambda () (org-kit::babel-tangle-mode "z+x"))))
           (invalid-permissions
             (babel-tangle-test-condition
              (lambda () (org-kit::babel-tangle-mode "u+z"))))
           (invalid-mode-type
             (babel-tangle-test-condition
              (lambda () (org-kit::babel-tangle-mode nil)))))
      (flet ((expect-reason (condition reason)
               (expect (typep condition 'org-babel-error) :to-be t)
               (when (typep condition 'org-babel-error)
                 (expect (org-babel-error-reason condition)
                         :to-be reason))))
        (with-soft-assertions
          (expect (write-plan-entry-content org-entry)
                  :to-equal (format nil "# org context~%body~%"))
          (expect-reason unsupported-comments-error :unsupported-comments-mode)
          (expect-reason invalid-comment-error :invalid-comment-renderer-result)
          (expect-reason invalid-link-error :invalid-comment-renderer-result)
          (expect-reason invalid-variable-error :invalid-variable-renderer-result)
          (expect (write-plan-entry-content tab-entry)
                  :to-equal (format nil "first~%    second~%"))
          (expect (typep add-piece-error 'type-error) :to-be t)
          (expect-reason invalid-who :invalid-tangle-mode)
          (expect-reason invalid-permissions :invalid-tangle-mode)
          (expect (typep invalid-mode-type 'type-error) :to-be t)
          (expect (org-kit::babel-tangle-mode "g+w") :to-be #o664)
          (expect (org-kit::babel-tangle-parse-symbolic-mode "g=u" #o640)
                  :to-be #o660)
          (expect (org-kit::babel-tangle-parse-symbolic-mode "a+X" #o644)
                  :to-be #o644)
          (expect (org-kit::babel-tangle-parse-symbolic-mode "a+X" #o740)
                  :to-be #o751)))))

)
(describe "Babel tangle mode and repeated-file boundaries"
  (it "covers destination validation and all permission-source groups"
    (let* ((default-document
             (babel-tangle-test-document
              (format nil "#+BEGIN_SRC text~%body~%#+END_SRC")))
           (default-block (first (babel-tangle-test-blocks default-document)))
           (type-error
             (babel-tangle-test-condition
              (lambda ()
                (org-kit::babel-tangle-target-path
                 default-document default-block nil 42))))
           (symbolic-document
             (babel-tangle-test-document
              (format nil
                      "#+BEGIN_SRC text :tangle symbolic.txt :tangle-mode u=g,u=o,g=u,o=u~%body~%#+END_SRC")))
           (symbolic-entry
             (babel-tangle-test-entry
              (babel-tangle symbolic-document) "symbolic.txt"))
           (file-mode-document
             (babel-tangle-test-document
              (format nil
                      "#+BEGIN_SRC text :tangle file-mode.txt :tangle-mode rw-rw-rw-~%body~%#+END_SRC")))
           (file-mode-entry
             (babel-tangle-test-entry
              (babel-tangle file-mode-document) "file-mode.txt")))
      (with-soft-assertions
        (expect (typep type-error 'type-error) :to-be t)
        (expect (write-plan-entry-mode symbolic-entry) :to-be #o444)
        (expect (write-plan-entry-mode file-mode-entry) :to-be #o666))))
  (it "keeps the first shebang, mkdirp, and mode for repeated paths"
    (let* ((document
             (babel-tangle-test-document
              (babel-tangle-test-lines
               "#+BEGIN_SRC shell :tangle repeated.sh :mkdirp no :shebang \"#!/bin/sh\" :tangle-mode u+x"
               "first"
               "#+END_SRC"
               ""
               "#+BEGIN_SRC shell :tangle repeated.sh :mkdirp no :shebang \"#!/usr/bin/env sh\" :tangle-mode u-x"
               "second"
               "#+END_SRC")))
           (entry
             (babel-tangle-test-entry (babel-tangle document) "repeated.sh"))
           (content (write-plan-entry-content entry)))
      (with-soft-assertions
        (expect (write-plan-entry-mode entry) :to-be #o744)
        (expect (write-plan-entry-create-directories-p entry) :to-be nil)
        (expect (search "#!/bin/sh" content) :to-be 0)
        (expect (search "#!/usr/bin/env sh" content) :to-be nil)
        (expect (search "first" content) :not :to-be nil)
        (expect (search "second" content) :not :to-be nil))))
  (it "covers derived pathname, non-string language, and mode boundaries"
    (let* ((document
             (babel-tangle-test-document
              (format nil
                      "#+BEGIN_SRC emacs-lisp :tangle yes~%body~%#+END_SRC")
              :source-name #P"guide.org"))
           (entry
             (aref (write-plan-entries (babel-tangle document)) 0))
           (mode-error
             (babel-tangle-test-condition
              (lambda ()
                (org-kit::babel-tangle-mode "rwsr-sr-x"))))
           (compiled-regexp (cl-regex-kit:compile-regex ".*")))
      (with-soft-assertions
        (expect (write-plan-entry-path entry) :to-equal "guide.el")
        (expect
         (org-kit::babel-tangle-language-matches-p 42 compiled-regexp)
         :to-be
         nil)
        (expect (org-kit::babel-tangle-ensure-newline
                 (format nil "#!/bin/sh~%"))
                :to-equal
                (format nil "#!/bin/sh~%"))
        (expect (typep mode-error 'org-kit:org-babel-error) :to-be t)
        (when (typep mode-error 'org-kit:org-babel-error)
          (expect (org-kit:org-babel-error-reason mode-error)
                  :to-be
                  :invalid-tangle-mode)))))
  (it "covers empty source names and invalid mode spellings"
    (let* ((empty-source-document
             (babel-tangle-test-document "" :source-name ""))
           (empty-mode-error
             (babel-tangle-test-condition
              (lambda () (org-kit::babel-tangle-mode ""))))
           (invalid-octal
             (babel-tangle-test-condition
              (lambda () (org-kit::babel-tangle-mode "o0788"))))
           (invalid-file-mode
             (babel-tangle-test-condition
              (lambda () (org-kit::babel-tangle-mode "rwsr-sr-t"))))
           (negative-mode
             (babel-tangle-test-condition
              (lambda () (org-kit::babel-tangle-mode -1)))))
      (with-soft-assertions
        (expect (org-kit::babel-tangle-source-relative-path empty-source-document)
                :to-be nil)
        (expect (org-kit::babel-tangle-ensure-newline "")
                :to-equal (string #\Newline))
        (expect (typep empty-mode-error 'org-kit:org-babel-error) :to-be t)
        (expect (typep invalid-octal 'org-kit:org-babel-error) :to-be t)
        (expect (typep invalid-file-mode 'org-kit:org-babel-error) :to-be t)
        (expect (typep negative-mode 'org-kit:org-babel-error) :to-be t)
        (when (typep invalid-file-mode 'org-kit:org-babel-error)
          (expect (org-kit:org-babel-error-reason invalid-file-mode)
                  :to-be :invalid-tangle-mode))))))
