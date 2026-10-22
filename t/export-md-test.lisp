;;;; t/export-md-test.lisp
(in-package #:cl-org-kit/test)

(defun export-md-test-fixture-path (relative)
  (asdf:system-relative-pathname
   "cl-org-kit" (concatenate 'string "t/fixtures/export/" relative)))

(defun export-md-test-read-text (relative)
  (uiop:read-file-string (export-md-test-fixture-path relative)))

(defun export-md-test-read-octets (relative)
  (with-open-file (stream (export-md-test-fixture-path relative)
                          :direction :input :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (octets (make-array length :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence octets stream) length)
        (error "Unable to read all Markdown fixture bytes from ~A" relative))
      octets)))

(progn
  (defun export-md-test-catch-resource-limit (function)
    (handler-case (progn (funcall function) nil)
      (org-resource-limit-error (condition) condition)))
  (describe "Markdown export edge behavior"
    (it "renders headline objects, options, and deep-level fallback like ox-md"
      (let* ((source
               (format nil
                       "#+OPTIONS: toc:nil num:2 pri:t tags:nil todo:t~%* TODO [#A] Parent *bold* /italic/ :visible:~%******* DONE [#B] Deep _title_~%Body line.~%"))
             (output (export-org (parse-org source) :md)))
        (with-soft-assertions
          (expect (search "# TODO [#A] Parent **bold** *italic*" output)
                  :to-be-truthy)
          (expect (search "-   DONE [#B] Deep <span class=\"underline\">title</span>"
                          output)
                  :to-be-truthy)
          (expect (search "    Body line." output) :to-be-truthy)
          (expect (search "visible" output) :to-be nil))))
    (it "preserves destination-like syntax inside a rendered code-span link label"
      (let* ((label (concatenate 'string (string (code-char 96))
                                 "label ](<x>)"
                                 (string (code-char 96))))
             (destination "<guide.md>")
             (rendered (format nil "[~A](~A)" label destination))
             (expected (format nil "[~A](guide.md)" label)))
        (expect (org-kit::export-md-strip-link-destination-angles rendered destination)
                :to-equal expected)))
    (it "renders heading TODO, priority, and tags according to options"
      (let* ((source
               (format nil
                       "#+OPTIONS: toc:nil num:nil pri:nil tags:t todo:nil~%* TODO [#A] Tagged :visible:~%"))
             (output (export-org (parse-org source) :md)))
        (with-soft-assertions
          (expect (search "# Tagged     :visible:" output) :to-be-truthy)
          (expect (search "TODO" output) :to-be nil)
          (expect (search "[#A]" output) :to-be nil))))
    (it "merges a special block class with ATTR_HTML classes"
      (let* ((source
               (format nil
                       "#+ATTR_HTML: :id stable-special :class custom~%#+begin_note~%Special class content.~%#+end_note~%"))
             (output (export-org (parse-org source) :md)))
        (expect (search "<div id=\"stable-special\" class=\"custom note\">"
                        output)
                :to-be-truthy)))
    (it "renders descriptive terms and keeps ordered lists numbered with num:nil"
      (let* ((source
               (format nil "- Term :: Definition body.~%"))
             (output (export-org (parse-org source) :md))
             (num-nil-source
               (format nil
                       "#+OPTIONS: num:nil~%1. First item~%2. Second item~%"))
             (num-nil-output
               (export-org (parse-org num-nil-source) :md))
             (numbered-source
               (format nil
                       "1. First numbered item~%2. Second numbered item~%"))
             (numbered-output
               (export-org (parse-org numbered-source) :md)))
        (with-soft-assertions
          (expect (search "-   **Term:** Definition body." output)
                  :to-be-truthy)
          (expect (search
                   (format nil
                           "1.   First item~%2.   Second item")
                   num-nil-output)
                  :to-be-truthy)
          (expect (search
                   (format nil
                           "1.   First numbered item~%2.   Second numbered item")
                   numbered-output)
                  :to-be-truthy))))
    (it "escapes a paragraph-leading literal hash once"
      (let* ((source (format nil "~C#literal paragraph~%" #\\))
             (output (export-org (parse-org source) :md))
             (once (format nil "~C~C#literal paragraph" #\\ #\\))
             (thrice (format nil "~C~C~C#literal paragraph"
                             #\\ #\\ #\\)))
        (with-soft-assertions
          (expect (search once output) :to-be-truthy)
          (expect (search thrice output) :to-be nil))))
    (it "honors the raw HTML policy while matching ox-md's explicit raw default"
      (let* ((source
               (format nil
                       "#+begin_export html~%<aside>policy marker</aside>~%#+end_export~%@@html:<u>snippet marker</u>@@~%"))
             (document (parse-org source))
             (denied (export-org document :md))
             (allowed (export-org document :md :allow-raw-html t)))
        (with-soft-assertions
          (expect (search "<aside>policy marker</aside>" denied) :to-be nil)
          (expect (search "&lt;aside&gt;policy marker&lt;/aside&gt;" denied)
                  :to-be-truthy)
          (expect (search "<aside>policy marker</aside>" allowed)
                  :to-be-truthy)
          (expect (search "<u>snippet marker</u>" allowed)
                  :to-be-truthy))))
    (it "validates inline :exports while preserving default, code, and results behavior"
      (let* ((default-source
               (format nil "src_emacs-lisp{(+ 1 2)}~%"))
             (results-source
               (format nil "src_emacs-lisp[:results raw]{(+ 1 2)}~%"))
             (code-source
               (format nil "src_emacs-lisp[:results raw :exports code]{(+ 1 2)}~%"))
             (results-then-code-source
               (format nil
                       "src_emacs-lisp[:exports results :exports code]{(+ 1 2)}~%"))
             (code-then-results-source
               (format nil
                       "src_emacs-lisp[:exports code :exports results]{(+ 1 2)}~%"))
             (invalid-source
               (format nil "src_emacs-lisp[:exports foo]{(+ 1 2)}~%"))
             (default-output
               (export-org (parse-org default-source) :md))
             (results-output
               (export-org (parse-org results-source) :md))
             (code-output
               (export-org (parse-org code-source) :md))
             (results-then-code-output
               (export-org (parse-org results-then-code-source) :md))
             (code-then-results-output
               (export-org (parse-org code-then-results-source) :md))
             (invalid-error
               (handler-case
                   (progn (export-org (parse-org invalid-source) :md) nil)
                 (org-export-unsupported-syntax (condition)
                   (list (org-export-unsupported-syntax-backend condition)
                         (org-export-unsupported-syntax-feature condition))))))
        (with-soft-assertions
          (expect (search "`(+ 1 2)`" default-output) :to-be-truthy)
          (expect (search "(+ 1 2)" results-output) :to-be nil)
          (expect (search "`(+ 1 2)`" code-output) :to-be-truthy)
          (expect (search "`(+ 1 2)`" results-then-code-output)
                  :to-be-truthy)
          (expect (search "`(+ 1 2)`" code-then-results-output) :to-be nil)
          (expect (and (consp invalid-error)
                       (eq (first invalid-error) :md)
                       (eq (second invalid-error) :src-header-argument))
                  :to-be-truthy))))))

(describe "Markdown export"
  (it "matches all eight pinned Org 9.8.10 Markdown oracle cases byte for byte"
    (let* ((rows
             (remove-if-not
              (lambda (row)
                (member (first row)
                        (list "metadata-selection" "exclude-tags-override"
                              "footnotes-macros-links" "include"
                              "source-blocks" "md-elements"
                              "org-elements" "backend-extended-elements")
                        :test (function string=)))
              (mapcar
               (lambda (line)
                 (loop with start = 0
                       for tab = (position #\Tab line :start start)
                       collect (subseq line start tab)
                       while tab
                         do (setf start (1+ tab))))
               (nthcdr 2 (uiop:read-file-lines
                           (export-md-test-fixture-path "oracle/manifest.tsv"))))))
           (case-names nil))
      (with-soft-assertions
        (expect (length rows) :to-equal 8)
        (expect (mapcar #'length rows) :to-equal '(3 3 3 3 3 3 3 3))
        (expect (export-backend-derived-p :md :gfm) :to-be t)
        (dolist (row rows)
          (destructuring-bind (case source include) row
            (push case case-names)
            (let* ((source-path (concatenate 'string "oracle/corpus/" source))
                   (expected-path (concatenate 'string "oracle/md/" case ".md"))
                   (metadata-path (concatenate 'string "oracle/md/" case ".meta"))
                   (source-text (export-md-test-read-text source-path))
                   (metadata (export-md-test-read-text metadata-path))
                   (expected (export-md-test-read-octets expected-path))
                   (include-calls nil)
                   (resolver
                    (unless (string= include "-")
                      (lambda (path parent-source)
                        (declare (ignore parent-source))
                        (push path include-calls)
                        (if (string= path include)
                            (export-md-test-read-text
                             (concatenate 'string "oracle/corpus/" include))
                            (error "Unexpected Markdown oracle include path ~A" path))))))
              (expect (plusp (length source-text)) :to-be t)
              (expect (plusp (length metadata)) :to-be t)
              (expect (plusp (length expected)) :to-be t)
              (expect (search "org=9.8.10" metadata) :to-be-truthy)
              (expect (search "backend=md" metadata) :to-be-truthy)
              (when
                  (and (plusp (length source-text))
                       (plusp (length metadata))
                       (plusp (length expected))
                       (search "org=9.8.10" metadata)
                       (search "backend=md" metadata))
                (let* ((document (parse-org source-text :source-name source))
                       (actual
                        (export-org document
                                    :md
                                    :allow-raw-html
                                    t
                                    :include-resolver
                                    resolver
                                    :output-type
                                    :octets)))
                  (expect actual :to-equalp expected)
                  (expect (reverse include-calls)
                          :to-equal
                          (if (string= include "-")
                              nil
                              (list include))))))))
        (expect (reverse case-names)
                :to-equal '("metadata-selection" "exclude-tags-override"
                       "footnotes-macros-links"
                       "include"
                       "source-blocks"
                       "md-elements"
                       "org-elements"
                       "backend-extended-elements")))))

  (it "enforces output character and UTF-8 octet limits"
    (let* ((source "é")
           (baseline (export-org (parse-org source) :md))
           (baseline-octets
             (cl-codec-kit:string-to-octets baseline :encoding :utf-8))
           (length-error
             (export-md-test-catch-resource-limit
              (lambda ()
                (export-org
                 (parse-org source
                            :limits
                            (make-limits :max-output-length
                                         (1- (length baseline))))
                 :md))))
           (octet-error
             (export-md-test-catch-resource-limit
              (lambda ()
                (export-org
                 (parse-org source
                            :limits
                            (make-limits :max-output-octets
                                         (1- (length baseline-octets))))
                 :md :output-type :octets)))))
      (with-soft-assertions
        (expect (typep length-error 'org-resource-limit-error) :to-be t)
        (when (typep length-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name length-error)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual length-error)
                  :to-be (length baseline)))
        (expect (typep octet-error 'org-resource-limit-error) :to-be t)
        (when (typep octet-error 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name octet-error)
                  :to-be :max-output-octets)
          (expect (org-resource-limit-error-actual octet-error)
                  :to-be (length baseline-octets))))))
  (it "checks code-span output limits before materializing the span"
    (let ((failure
            (export-md-test-catch-resource-limit
             (lambda ()
               (export-org
                (parse-org "=x="
                           :limits (make-limits :max-output-length 2))
                :md)))))
      (with-soft-assertions
        (expect (typep failure (quote org-resource-limit-error)) :to-be t)
        (when (typep failure (quote org-resource-limit-error))
          (expect (org-resource-limit-error-limit-name failure)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual failure) :to-be 3)))))
  (it "preflights each footnote body against cumulative output length"
    (let* ((source
             (format nil
                     "Body text[fn:limit].~%~%[fn:limit] =cumulative body=~%"))
           (baseline (export-org (parse-org source) :md))
           (limit (- (length baseline) 2))
           (failure
             (export-md-test-catch-resource-limit
              (lambda ()
                (export-org
                 (parse-org source
                            :limits
                            (make-limits :max-output-length limit))
                 :md)))))
      (with-soft-assertions
        (expect (> (length baseline) 2) :to-be t)
        (expect (typep failure 'org-resource-limit-error) :to-be t)
        (when (typep failure 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name failure)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-actual failure)
                  :to-be (1- (length baseline)))))))
  (it "renders Markdown references, inline formats, tasks, and footnotes"
    (let* ((tick (string (code-char 96)))
           (source
             (format nil
                     "#+OPTIONS: toc:t num:2 tags:t todo:t pri:t~%* TODO [#A] Guide :public:~%:PROPERTIES:~%:CUSTOM_ID: guide-id~%:END:~%First hard break  ~%Second line.~%Claim[fn:note].~%Math ~C[x+y~C].~%Ticks =left~Cright=.~%- [ ] Open~%- [X] Done~%- [-] In progress~%- *Term* :: Definition body.~%10. Tenth~%11. Eleventh~%[fn:note] = note body =~%"
                     (code-char 92) (code-char 92) (code-char 96)))
           (output (export-org (parse-org source) :md)))
      (with-soft-assertions
        (expect (search "# Table of Contents" output) :to-be-truthy)
        (expect (search "#guide-id" output) :to-be-truthy)
        (expect (search ":public:" output) :to-be-truthy)
        (expect (search "<a id=\"guide-id\"></a>" output) :to-be-truthy)
        (expect (search "First hard break  " output) :to-be-truthy)
        (expect (search "Second line." output) :to-be-truthy)
        (expect (search "$$x+y$$" output) :to-be-truthy)
        (expect
         (search (concatenate `string tick tick "left" tick "right" tick tick)
                 output)
         :to-be-truthy)
        (expect (search "-   [ ] Open" output) :to-be-truthy)
        (expect (search "-   [X] Done" output) :to-be-truthy)
        (expect (search "-   [-] In progress" output) :to-be-truthy)
        (expect (search "Definition body." output) :to-be-truthy)
        (expect (search "10.   Tenth" output) :to-be-truthy)
        (expect (search "# Footnotes" output) :to-be-truthy)
        (expect (search "note body" output) :to-be-truthy))))
  (it "renders safe and rejected Markdown link handler results"
    (let ((link-registered-p nil)
          (image-registered-p nil))
      (unwind-protect
           (progn
             (register-export-link-type
              :md "md-test"
              (lambda (path description frame context)
                (declare (ignore frame context))
                (make-export-link-result
                 :kind :link
                 :destination
                 (if (string= path "unsafe")
                     "javascript:alert(1)"
                     "https://example.test/safe")
                 :description description)))
             (setf link-registered-p t)
             (register-export-link-type
              :md "md-image"
              (lambda (path description frame context)
                (declare (ignore path frame context))
                (make-export-link-result
                 :kind :image
                 :destination "assets/logo.png"
                 :description description)))
             (setf image-registered-p t)
             (let ((output
                     (export-org
                      (parse-org
                       (format nil
                               "[[md-test:safe][Safe]]~%[[md-test:unsafe][Blocked]]~%[[md-image:asset.png][Logo]]~%[[file:guide.org][Guide]]~%"))
                      :md)))
               (with-soft-assertions
                 (expect
                  (search "[Safe](https://example.test/safe)" output)
                  :to-be-truthy)
                 (expect (search "Blocked" output) :to-be-truthy)
                 (expect (search "javascript:alert(1)" output) :to-be nil)
                 (expect (search "![Logo](assets/logo.png)" output)
                         :to-be-truthy)
                 (expect (search "[Guide](guide.md)" output)
                         :to-be-truthy))))
        (when image-registered-p
          (unregister-export-link-type :md "md-image"))
        (when link-registered-p
          (unregister-export-link-type :md "md-test")))))
  (it "renders generated special block attributes and inline tasks"
    (let* ((source
             (format nil
                     "#+begin_note~%Note body.~%#+end_note~%*************** TODO Inline task~%Task body.~%*************** END~%"))
           (output (export-org (parse-org source) :md)))
      (with-soft-assertions
        (expect (search "id=\"" output) :to-be-truthy)
        (expect (search "class=\"note\"" output) :to-be-truthy)
        (expect (search "Note body." output) :to-be-truthy)
        (expect (search "<div class=\"inlinetask\">" output) :to-be-truthy)
        (expect (search "Inline task" output) :to-be-truthy)
        (expect (search "Task body." output) :to-be-truthy)))))
