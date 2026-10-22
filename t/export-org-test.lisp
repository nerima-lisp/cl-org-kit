;;;; t/export-org-test.lisp
(in-package #:cl-org-kit/test)

(defun export-org-test-fixture-path (relative)
  (asdf:system-relative-pathname
   "cl-org-kit" (concatenate 'string "t/fixtures/export/" relative)))

(defun export-org-test-read-text (relative)
  (uiop:read-file-string (export-org-test-fixture-path relative)))

(defun export-org-test-read-octets (relative)
  (with-open-file (stream (export-org-test-fixture-path relative)
                          :direction :input :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (octets (make-array length :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence octets stream) length)
        (error "Unable to read all bytes from Org fixture ~A" relative))
      octets)))

(defun export-org-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(describe "E4 Org backend export"
  (it "matches all eight Org 9.8.10 oracle outputs byte for byte"
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
               (nthcdr 2
                (uiop:read-file-lines
                 (export-org-test-fixture-path "oracle/manifest.tsv"))))))
           (case-names nil))
      (with-soft-assertions
        (expect (length rows) :to-equal 8)
        (expect (mapcar #'length rows) :to-equal '(3 3 3 3 3 3 3 3))
        (expect (org-kit::export-backend-registered-p :org) :to-be t)
        (dolist (row rows)
          (destructuring-bind (case source include) row
            (push case case-names)
            (let* ((source-path
                     (concatenate 'string "oracle/corpus/" source))
                   (expected-path
                     (concatenate 'string "oracle/org/" case ".org"))
                   (source-text
                     (export-org-test-read-text source-path))
                   (expected
                     (export-org-test-read-octets expected-path))
                   (metadata
                     (export-org-test-read-text
                      (concatenate 'string "oracle/org/" case ".meta")))
                   (document
                     (parse-org source-text :source-name source))
                   (resolver-calls nil)
                   (resolver
                     (unless (string= include "-")
                       (lambda (path parent-source)
                         (push (list path parent-source) resolver-calls)
                         (unless (and (string= path include)
                                      (string= parent-source source))
                           (error "Unexpected include resolver request: ~S from ~S"
                                  path parent-source))
                         (export-org-test-read-text
                          (concatenate 'string "oracle/corpus/" path)))))
                   (actual
                     (export-org document :org
                                 :include-resolver resolver
                                 :output-type :octets)))
              (expect (plusp (length source-text)) :to-be t)
              (expect (plusp (length expected)) :to-be t)
              (expect (plusp (length metadata)) :to-be t)
              (expect (search "emacs=31.1" metadata) :to-be-truthy)
              (expect (search "org=9.8.10" metadata) :to-be-truthy)
              (expect (search "backend=org" metadata) :to-be-truthy)
              (expect (search (format nil "source=~A" source) metadata)
                      :to-be-truthy)
              (expect
               (search
                (format nil "include-source=~A"
                        (if (string= include "-") "none" include))
                metadata)
               :to-be-truthy)
              (expect (reverse resolver-calls)
                      :to-equal
                      (if (string= include "-")
                          nil
                          (list (list include source))))
              (expect actual :to-equalp expected))))
        (expect
         (reverse case-names)
         :to-equal
         '("metadata-selection" "exclude-tags-override"
           "footnotes-macros-links" "include" "source-blocks"
           "md-elements" "org-elements" "backend-extended-elements")))))

  (it "retains only selected properties in a property drawer"
    (let* ((source
             (format nil
                     "#+OPTIONS: prop:(CUSTOM_ID)~%* Property selection~%:PROPERTIES:~%:CUSTOM_ID: selected-value~%:PRIVATE: hidden-value~%:END:~%"))
           (output (export-org (parse-org source) :org)))
      (with-soft-assertions
        (expect (search ":PROPERTIES:" output) :to-be-truthy)
        (expect (search ":CUSTOM_ID: selected-value" output)
                :to-be-truthy)
        (expect (search ":PRIVATE: hidden-value" output) :to-be nil)
        (expect (search ":END:" output) :to-be-truthy))))

  (it "distinguishes same-day time ranges with warning adjustments from date ranges"
    (let* ((time-range
             (export-org
              (parse-org "<2026-10-22 10:00-11:00 --2d>")
              :org))
           (date-range
             (export-org
              (parse-org "<2026-10-22>--<2026-10-24>")
              :org))
           (warning-date-range
             (export-org
              (parse-org "<2026-10-22 --2d>--<2026-10-24>")
              :org)))
      (with-soft-assertions
        (expect time-range :to-equal
                (format nil "<2026-10-22 Thu 10:00-11:00 --2d>~%"))
        (expect date-range :to-equal
                (format nil "<2026-10-22 Thu>--<2026-10-24 Sat>~%"))
        (expect warning-date-range :to-equal
                (format nil "<2026-10-22 Thu --2d>--<2026-10-24 Sat --2d>~%")))))

  (it "enforces output character and UTF-8 octet limits"
    (let* ((source "é output")
           (baseline (export-org (parse-org source) :org))
           (baseline-octets
             (cl-codec-kit:string-to-octets baseline :encoding :utf-8))
           (below-length
             (export-org-test-catch-error
              (lambda ()
                (export-org
                 (parse-org
                  source
                  :limits (make-limits :max-output-length 1))
                 :org))))
           (below-octets
             (export-org-test-catch-error
              (lambda ()
                (export-org
                 (parse-org
                  source
                  :limits (make-limits :max-output-octets 1))
                 :org :output-type :octets)))))
      (with-soft-assertions
        (expect (plusp (length baseline)) :to-be t)
        (expect (plusp (length baseline-octets)) :to-be t)
        (expect (typep below-length 'org-resource-limit-error) :to-be t)
        (when (typep below-length 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below-length)
                  :to-be :max-output-length)
          (expect (org-resource-limit-error-limit below-length) :to-be 1)
          (expect (> (org-resource-limit-error-actual below-length) 1) :to-be t))
        (expect (typep below-octets 'org-resource-limit-error) :to-be t)
        (when (typep below-octets 'org-resource-limit-error)
          (expect (org-resource-limit-error-limit-name below-octets)
                  :to-be :max-output-octets)
          (expect (org-resource-limit-error-limit below-octets) :to-be 1)
          (expect (> (org-resource-limit-error-actual below-octets) 1) :to-be t)))))
  (it "renders registered Org links and preserves unmatched source links"
    (let ((handler-registered-p nil))
      (unwind-protect
           (progn
             (register-export-link-type
              :org "export-test"
              (lambda (path description frame context)
                (declare (ignore path frame context))
                (make-export-link-result
                 :kind :link
                 :destination "https://example.test/target"
                 :description description)))
             (setf handler-registered-p t)
             (let ((output
                     (export-org
                      (parse-org
                       (format nil
                               "[[export-test:target][*Styled*]]~%[[export-test:bare]]~%[[https://example.test/fallback][Fallback]]"))
                      :org)))
               (with-soft-assertions
                 (expect
                  (search "[[https://example.test/target][*Styled*]]" output)
                  :to-be-truthy)
                 (expect
                  (search "[[https://example.test/target]]" output)
                  :to-be-truthy)
                 (expect
                  (search "[[https://example.test/fallback][Fallback]]" output)
                  :to-be-truthy))))
        (when handler-registered-p
          (unregister-export-link-type :org "export-test")))))
  (it "applies Org metadata, planning, clock, priority, tag, and property options"
    (let* ((content
             (format nil
                     "#+TITLE: Export guide~%#+AUTHOR: Ada Example~%#+EMAIL: ada@example.test~%#+CREATOR: Toolchain~%* TODO [#B] Task :private:~%SCHEDULED: <2026-10-20 Tue>~%:PROPERTIES:~%:CUSTOM_ID: selected-id~%:PRIVATE: hidden-value~%:END:~%CLOCK: [2026-10-20 Tue 09:00]--[2026-10-20 Tue 09:30]~%Body~%"))
           (enabled
             (export-org
              (parse-org
               (format nil "#+OPTIONS: author:t email:t creator:comment p:t c:t pri:t prop:(CUSTOM_ID) tags:nil todo:nil~%~A" content))
              :org))
           (defaults (export-org (parse-org content) :org)))
      (with-soft-assertions
        (expect (search "#+author: Ada Example" enabled) :to-be-truthy)
        (expect (search "#+email: ada@example.test" enabled) :to-be-truthy)
        (expect (search "#+creator: Toolchain" enabled) :to-be-truthy)
        (expect (search "SCHEDULED: <2026-10-20 Tue>" enabled) :to-be-truthy)
        (expect (search "CLOCK: [2026-10-20 Tue 09:00]" enabled) :to-be-truthy)
        (expect (search ":CUSTOM_ID: selected-id" enabled) :to-be-truthy)
        (expect (search "hidden-value" enabled) :to-be nil)
        (expect (search "[#B]" enabled) :to-be-truthy)
        (expect (search "TODO" enabled) :to-be nil)
        (expect (search ":private:" enabled) :to-be nil)
        (expect (search "TODO" defaults) :to-be-truthy)
        (expect (search ":private:" defaults) :to-be-truthy)
        (expect (search "SCHEDULED:" defaults) :to-be nil)
        (expect (search "CLOCK:" defaults) :to-be nil)
        (expect (search ":CUSTOM_ID:" defaults) :to-be nil))))
  (it "preserves include boundaries, source code whitespace, keyword cases, and timestamps"
    (let* ((source
             (format nil
                     "* Parent~%Before include.~%#+INCLUDE: \"child.org\"~%After include.~%#+begin_src text~%    first~%      second~%#+end_src"))
           (included
             (export-org
              (parse-org source :source-name "root.org")
              :org
              :include-resolver
              (lambda (path parent-source)
                (declare (ignore parent-source))
                (if (string= path "child.org")
                    (format nil "* Included~%Child body.~%")
                    (error "Unexpected include path ~A" path)))))
           (keywords
             (export-org
              (parse-org
               (format nil "#+RESULTS:~%#+RESULTS: Value~%#+CUSTOM_KEY: Visible~%"))
              :org))
           (timestamps
             (export-org
              (parse-org
               "<2026-10-20 Tue> [2026-10-21 Wed] <2026-10-25 Sun> <%%(diary-float t 4 2)>")
              :org)))
      (with-soft-assertions
        (expect (search "Before include." included) :to-be-truthy)
        (expect (search "Child body." included) :to-be-truthy)
        (expect (search "After include." included) :to-be-truthy)
        (expect (search "  first" included) :to-be-truthy)
        (expect (search "    second" included) :to-be-truthy)
        (expect (search "#+results: " keywords) :to-be-truthy)
        (expect (search "#+results: Value" keywords) :to-be-truthy)
        (expect (search "#+custom_key: Visible" keywords) :to-be-truthy)
        (expect (search "<2026-10-20 Tue>" timestamps) :to-be-truthy)
        (expect (search "[2026-10-21 Wed]" timestamps) :to-be-truthy)
        (expect (search "<2026-10-25 Sun>" timestamps) :to-be-truthy)
        (expect
         (search "<%%(diary-float t 4 2)>" timestamps)
         :to-be-truthy)))))