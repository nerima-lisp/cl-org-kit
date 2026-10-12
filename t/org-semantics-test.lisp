;;;; t/org-semantics-test.lisp
(in-package #:cl-org-kit/test)

(describe "Org setting and heading semantics"
  (it "resolves source setting names to document settings keys"
    (with-soft-assertions
      (expect (mapcar #'org-kit::org-setting-key
                      '("TODO" "seq_todo" "TYP_TODO" "TODO[1]"
                        "SEQ_TODO[2]" "TYP_TODO[3]" :startup "category"
                        "FILETAGS" "INLINETASK-MIN-LEVEL" "LINK"
                        "LINK-ABBREVS" "ODD-LEVELS-ONLY-P" "TAG-ALIST"
                        :tags "TAGS" "TODO-KEYWORDS" :todo-keywords))
              :to-equal
              '(:todo-keywords :todo-keywords :todo-keywords :todo-keywords
                :todo-keywords :todo-keywords :startup :category :file-tags
                :inlinetask-min-level :link-abbrevs :link-abbrevs
                :odd-levels-only-p :tag-alist :tag-alist :tag-alist
                :todo-keywords :todo-keywords))
      (expect (org-kit::org-setting-key "TODO[") :to-be nil)
      (expect (org-kit::org-setting-key "unknown") :to-be nil)
      (expect (org-kit::org-setting-key nil) :to-be nil)
      (expect (org-kit::org-setting-key 42) :to-be nil)))

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
        (expect unknown-default :to-equal :fallback)))

  (it "uses the last recognized odd or oddeven startup option"
    (let ((document
            (parse-org (format nil "#+STARTUP: odd~%#+STARTUP: oddeven~%"))))
      (with-soft-assertions
        (expect (org-kit::org-setting-value document :startup)
                :to-equal '("odd" "oddeven"))
        (expect (org-kit::org-setting-value document :odd-levels-only-p)
                :to-be nil)
        (expect (org-kit::org-startup-odd-levels-p
                 '("oddeven" "odd" :unknown "showall"))
                :to-be t)
        (expect (org-kit::org-startup-odd-levels-p
                 '("odd" "oddeven"))
                :to-be nil)
        (expect (org-kit::org-startup-odd-levels-p '(:unknown "showall"))
                :to-be nil)))
    (let ((document
            (parse-org ""
                       :settings '(:startup ("odd" :unknown "oddeven" "odd" "showall")))))
      (expect (org-kit::org-setting-value document :odd-levels-only-p)
              :to-be t)))

  (it "lets an explicit odd-level setting override startup tokens"
    (let ((enabled (parse-org "#+STARTUP: oddeven"
                              :settings '(:odd-levels-only-p t)))
          (disabled (parse-org "#+STARTUP: odd"
                               :settings '(:odd-levels-only-p nil))))
      (with-soft-assertions
        (expect (org-kit::org-setting-value enabled :odd-levels-only-p)
                :to-be t)
        (expect (org-kit::org-setting-value disabled :odd-levels-only-p)
                :to-be nil)
        (let ((document (make-document "")))
          (setf (document-settings document)
                '(:odd-levels-only-p nil :startup ("odd")))
          (expect (org-kit::org-setting-value document :odd-levels-only-p)
                  :to-be nil)))))

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
                :to-equal '(1 2 3 4)))))

  (it "requires positive integer source and logical levels"
    (let ((document (parse-org "")))
      (with-soft-assertions
        (signals type-error (org-kit::logical-heading-level document 0))
        (signals type-error (org-kit::logical-heading-level document "1"))
        (signals type-error (org-kit::source-heading-level document 0))
        (signals type-error (org-kit::source-heading-level document 1.5)))))))
