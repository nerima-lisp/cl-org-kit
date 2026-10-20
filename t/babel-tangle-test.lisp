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
                :to-equal "default body\n")
        (expect (write-plan-entry-content
                 (babel-tangle-test-entry plan "custom.lisp"))
                :to-equal "custom body\n")
        (expect (write-plan-entry-content
                 (babel-tangle-test-entry plan "inherited.el"))
                :to-equal "inherited body\n"))))

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
                :to-equal "(kept)\n\nskipped\n")
        (expect (write-plan-entry-content trim-entry)
                :to-equal "first\nsecond\n"))))

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
              "#+BEGIN_SRC shell :tangle special.sh :tangle-mode u+x\nbody\n#+END_SRC"))
           (special-entry
             (aref (write-plan-entries (babel-tangle special-document)) 0)))
      (with-soft-assertions
        (expect (write-plan-entry-path entry)
                :to-equal "babel-tangle-no-write-parent/nested/run.sh")
        (expect (write-plan-entry-content entry)
                :to-equal "#!/bin/sh\necho ready\n")
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
              "#+BEGIN_SRC text :tangle empty-comments.txt :comments link\nbody\n#+END_SRC"))
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
                "# source context\n# source link\nanswer=42\necho $answer\n# source ends here\n")
        (expect (write-plan-entry-content empty-comment-entry)
                :to-equal "body\n"))))

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
              :to-equal "before\nfragment\nafter\n")))

  (it "rejects unsafe paths and invalid language filters"
    (flet ((assert-condition (condition type reader reason)
             (expect (typep condition type) :to-be t)
             (when (typep condition type)
               (expect (funcall reader condition) :to-be reason))))
      (let* ((default-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text\nbody\n#+END_SRC"))
             (parent-path-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text :tangle a/../../escape.txt\nbody\n#+END_SRC"))
             (absolute-path-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text :tangle /tmp/outside.txt\nbody\n#+END_SRC"))
             (null-path-document
               (babel-tangle-test-document
                (format nil "#+BEGIN_SRC text :tangle bad~Cpath.txt~%body~%#+END_SRC"
                        #\Null)))
             (self-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text :tangle same.org\nbody\n#+END_SRC"
                :source-name "same.org"))
             (invalid-regexp-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text :tangle out.txt\nbody\n#+END_SRC"))
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
                "#+BEGIN_SRC text :tangle comments.txt :comments link\nbody\n#+END_SRC"))
             (variables-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text :tangle variables.txt :var x=1\nbody\n#+END_SRC"))
             (mode-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text :tangle mode.txt :tangle-mode o1000\nbody\n#+END_SRC"))
             (limited-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text :tangle limited.txt\nbody\n#+END_SRC"
                :limits (make-limits :max-write-plan-bytes 4)))
             (output-length-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text :tangle length.txt\nbody\n#+END_SRC"
                :limits (make-limits :max-output-length 4)))
             (output-octets-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text :tangle octets.txt\nbody\n#+END_SRC"
                :limits (make-limits :max-output-octets 4)))
             (expansion-document
               (babel-tangle-test-document
                "#+BEGIN_SRC text :tangle expanded.txt\nbody\n#+END_SRC"
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
                "#+BEGIN_SRC text :tangle nested/file.txt\nbody\n#+END_SRC"
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
                    :to-be :invalid-tangle-mode)))))))
