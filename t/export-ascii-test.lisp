;;;; t/export-ascii-test.lisp
(in-package #:cl-org-kit/test)

(defun export-ascii-test-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun export-ascii-test-source (&rest lines)
  (concatenate 'string (apply #'export-ascii-test-lines lines)
               (string #\Newline)))

(defun export-ascii-test-fixture (relative-path)
  (uiop:read-file-string
   (asdf:system-relative-pathname "cl-org-kit" relative-path)))

(defun export-ascii-test-find-node (root type)
  (or (when (eq (node-type root) type) root)
      (some (lambda (child) (export-ascii-test-find-node child type))
            (node-children root))))

(defun export-ascii-test-link-nodes (document)
  (let ((links nil))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :link)
                   (push node links)))
               (document-root document))
    (nreverse links)))

(defun export-ascii-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(defun export-ascii-test-copy-value (value)
  (cond
    ((stringp value) (copy-seq value))
    ((consp value)
     (cons (export-ascii-test-copy-value (car value))
           (export-ascii-test-copy-value (cdr value))))
    ((vectorp value)
     (map 'vector #'export-ascii-test-copy-value value))
    (t value)))

(defun export-ascii-test-document-snapshot (document)
  (let ((nodes nil))
    (map-nodes
     (lambda (node)
       (push (list node (node-type node) (node-parent node)
                   (copy-list (node-children node))
                   (node-begin node) (node-end node)
                   (node-contents-begin node) (node-contents-end node)
                   (node-post-blank node)
                   (export-ascii-test-copy-value (node-properties node)))
             nodes))
     (document-root document))
    (list (copy-seq (document-text document))
          (export-ascii-test-copy-value (document-source-name document))
          (export-ascii-test-copy-value (document-settings document))
          (nreverse nodes))))

(defun export-ascii-test-oracle-cases ()
  '(("metadata-selection" "metadata-selection.org" nil)
    ("exclude-tags-override" "exclude-tags-override.org" nil)
    ("footnotes-macros-links" "footnotes-macros-links.org" nil)
    ("include" "include.org" "included/fragment.org")
    ("source-blocks" "source-blocks.org" nil)))

(describe "ASCII export"
  (it "matches every checked-in ASCII oracle output through the public API"
    (let ((cases (export-ascii-test-oracle-cases)))
      (with-soft-assertions
        (expect (length cases) :to-be 5)
        (dolist (case cases)
          (let* ((name (first case))
                 (source-name (second case))
                 (include-path (third case))
                 (source
                   (export-ascii-test-fixture
                    (format nil "t/fixtures/export/oracle/corpus/~A"
                            source-name)))
                 (expected
                   (export-ascii-test-fixture
                    (format nil "t/fixtures/export/oracle/ascii/~A.txt" name)))
                 (document (parse-org source :source-name source-name))
                 (calls nil)
                 (resolver
                   (lambda (path parent-source)
                     (push (list path parent-source) calls)
                     (if (string= path include-path)
                         (export-ascii-test-fixture
                          "t/fixtures/export/oracle/corpus/included/fragment.org")
                         (error "Unexpected ASCII export include: ~A" path))))
                 (actual
                   (if include-path
                       (export-org document :ascii :include-resolver resolver)
                       (export-org document :ascii))))
            (expect (list name (plusp (length source)) (plusp (length expected)))
                    :to-equal (list name t t))
            (expect actual :to-equal expected)
            (expect (nreverse calls)
                    :to-equal (if include-path
                                  (list (list include-path source-name))
                                  nil)))))))

  (it "renders ordered and unordered lists deterministically"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "- bullet"
              "- second bullet"
              "1. ordered"
              "2. second ordered"))
           (expected
             (export-ascii-test-lines
              "  - bullet"
              "  - second bullet"
              "  1. ordered"
              "  2. second ordered"
              ""))
           (document (parse-org source))
           (first-output (export-org document :ascii))
           (second-output (export-org document :ascii)))
      (with-soft-assertions
        (expect first-output :to-equal expected)
        (expect second-output :to-equal first-output))))

  (it "aligns a Unicode table with the shared width and cell helpers"
    (let* ((source
             (export-ascii-test-source
              "| Name | Value |"
              "|------+-------|"
              "| 日本 | é |"))
           (document (parse-org source))
           (table (export-ascii-test-find-node (document-root document) :table))
           (rows (remove-if-not
                  (lambda (row)
                    (eq (node-property row :type) :standard))
                  (node-children table)))
           (output (export-org document :ascii))
           (expected
             (export-ascii-test-source
              "| Name | Value |"
              "+------+-------+"
              "| 日本 | é     |")))
      (multiple-value-bind (widths alignments)
          (org-kit::table-column-layout rows 2 72)
        (with-soft-assertions
          (expect (coerce widths 'list) :to-equal '(4 5))
          (expect (coerce alignments 'list) :to-equal '(:left :left))
          (expect (org-kit::table-string-width "日本") :to-be 4)
          (expect (org-kit::table-string-width "é") :to-be 1)
          (expect (org-kit::table-format-cell "日本" (aref widths 0) :left)
                  :to-equal " 日本 ")
          (expect (org-kit::table-format-cell "é" (aref widths 1) :left)
                  :to-equal " é     ")
          (expect output :to-equal expected)))))

  (it "honors caller metadata without changing the document"
    (let* ((source
             (export-ascii-test-source
              "#+TITLE: Document title"
              "#+AUTHOR: Document author"
              "#+DATE: 2026-10-14"
              "Body"))
           (document (parse-org source))
           (before (export-ascii-test-document-snapshot document))
           (output
             (export-org document :ascii
                         :context (make-export-context
                                   :title "Caller title"
                                   :author "Caller author"
                                   :date "2030-01-02"))))
      (with-soft-assertions
        (expect (search "CALLER TITLE" output) :not :to-be nil)
        (expect (search "DOCUMENT TITLE" output) :to-be nil)
        (expect (search "Caller author" output) :not :to-be nil)
        (expect (search "Document author" output) :to-be nil)
        (expect (search "2030-01-02" output) :not :to-be nil)
        (expect (search "2026-10-14" output) :to-be nil)
        (expect (export-ascii-test-document-snapshot document) :to-equal before))))

  (it "uses explicit metadata defaults without consulting ambient values"
    (let* ((empty-output
             (export-org (parse-org (export-ascii-test-source "Body")) :ascii))
           (title-only
             (export-org
              (parse-org (export-ascii-test-source "#+TITLE: Only title" "Body"))
              :ascii))
           (author-only
             (export-org
              (parse-org (export-ascii-test-source "#+AUTHOR: Only author" "Body"))
              :ascii))
           (date-only
             (export-org
              (parse-org (export-ascii-test-source "#+DATE: 2030-01-02" "Body"))
              :ascii))
           (author-and-date
             (export-org
              (parse-org
               (export-ascii-test-source
                "#+AUTHOR: Explicit author"
                "#+DATE: 2030-01-02"
                "Body"))
              :ascii))
           (disabled
             (export-org
              (parse-org
               (export-ascii-test-source
                "#+TITLE: Hidden title"
                "#+AUTHOR: Hidden author"
                "#+DATE: 2026-10-14"
                "#+OPTIONS: title:nil author:nil date:nil"
                "Body"))
              :ascii)))
      (with-soft-assertions
        (expect empty-output :to-equal (export-ascii-test-source "Body"))
        (expect (search "ONLY TITLE" title-only) :not :to-be nil)
        (expect (search "Only author" author-only) :not :to-be nil)
        (expect (search "2030-01-02" date-only) :not :to-be nil)
        (expect (search "Explicit author" author-and-date) :not :to-be nil)
        (expect (search "2030-01-02" author-and-date) :not :to-be nil)
        (expect (search "Hidden title" disabled) :to-be nil)
        (expect (search "Hidden author" disabled) :to-be nil)
        (expect (search "2026-10-14" disabled) :to-be nil)
        (expect (search "Body" disabled) :not :to-be nil))))

  (it "preserves explicit paragraph breaks and wraps at display width"
    (let* ((preserved
             (export-org
              (parse-org
               (export-ascii-test-source
                "#+OPTIONS: \\n:t"
                "First line"
                "Second line"))
              :ascii))
           (long-word (make-string 70 :initial-element #\x))
           (wrapped
             (export-org
              (parse-org
               (export-ascii-test-source
                (format nil "short ~A tail" long-word)))
              :ascii))
           (lines
             (with-input-from-string (stream wrapped)
               (loop for line = (read-line stream nil nil)
                     while line collect line))))
      (with-soft-assertions
        (expect preserved
                :to-equal (export-ascii-test-source "First line" "Second line"))
        (expect (every (lambda (line)
                         (<= (org-kit::table-string-width line) 72))
                       lines)
                :to-be t)
        (expect (search long-word wrapped) :not :to-be nil)
        (expect (search "tail" wrapped) :not :to-be nil))))

  (it "prints link labels, safe destinations, and one note per link"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil"
              "* Target"
              ":PROPERTIES:"
              ":CUSTOM_ID: target"
              ":END:"
              "* Links"
              "[[https://repeat.test][Repeat]] and [[https://repeat.test][Repeat]]."
              "[[https://bare.test]] and [[javascript:alert(1)][Unsafe]]."
              "[[#target][Internal]] and [[#target]]."))
           (output (export-org (parse-org source) :ascii))
           (note "[Repeat] <https://repeat.test>")
           (first-note (search note output))
           (second-note (and first-note
                             (search note output :start2 (1+ first-note)))))
      (with-soft-assertions
        (expect first-note :not :to-be nil)
        (expect second-note :to-be nil)
        (expect (search "<https://bare.test>" output) :not :to-be nil)
        (expect (search "[Unsafe]" output) :not :to-be nil)
        (expect (search "[Unsafe] <javascript:" output) :to-be nil)
        (expect (search "[Internal] See section 1" output) :not :to-be nil)
        (expect (search "Target" output) :not :to-be nil))))

  (it "rejects unsafe link destinations and keeps relative colon paths safe"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "* Links"
              "[[https://safe.test/path\\part][Backslash]]; [[file:private.txt][File]]; [[assets/topic:v2][Colon path]]."))
           (output (export-org (parse-org source) :ascii))
           (expected
             (export-ascii-test-lines
              "Links"
              "====="
              ""
              "  [Backslash]; [File]; [Colon path]."
              ""
              ""
              "[Colon path] <assets/topic:v2>"
              "")))
      (expect output :to-equal expected)))

  (it "applies link policy at whitespace, scheme, and abbreviation boundaries"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "#+LINK: safe https://docs.example/%s"
              "#+LINK: unsafe javascript:%s"
              "#+LINK: badpath https://docs.example\\%s"
              "[[ https://space.test/path][Leading]]; [[//cdn.test/path][Protocol relative]]; [[ftp://ftp.test/file][FTP]]; [[unknown:resource][Unknown]]; [[HTTPS://upper.test/path][Upper HTTPS]]; [[safe:chapter][Safe expansion]]; [[unsafe:payload][Unsafe expansion]]; [[/single-leading/path][Single slash]]; [[mailto:user@example.test][Mailto]]; [[news:group][News]]; [[badpath:chapter][Backslash expansion]]."))
           (document (parse-org source))
           (links (export-ascii-test-link-nodes document))
           (output (export-org document :ascii))
           (labels '("Leading" "Protocol relative" "FTP" "Unknown"
                     "Upper HTTPS" "Safe expansion" "Unsafe expansion"
                     "Single slash" "Mailto" "News" "Backslash expansion"))
           (unsafe-labels '("Leading" "Protocol relative" "FTP" "Unknown"
                            "Unsafe expansion" "News" "Backslash expansion")))
      (with-soft-assertions
        (expect (length links) :to-be 11)
        (expect (mapcar (lambda (label)
                          (not (null (search (format nil "[~A]" label) output))))
                        labels)
                :to-equal (make-list 11 :initial-element t))
        (expect (search "[Upper HTTPS] <HTTPS://upper.test/path>" output)
                :not :to-be nil)
        (expect (search "[Safe expansion] <https://docs.example/chapter>" output)
                :not :to-be nil)
        (expect (search "[Single slash] </single-leading/path>" output)
                :not :to-be nil)
        (expect (search "[Mailto] <mailto:user@example.test>" output)
                :not :to-be nil)
        (expect (mapcar (lambda (label)
                          (search (format nil "[~A] <" label) output))
                        unsafe-labels)
                :to-equal (make-list 7 :initial-element nil)))))

  (it "deduplicates notes for equal direct and abbreviated destinations"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "#+LINK: bare https://bare.test/%s"
              "[[bare:path][Bare link]] and [[https://bare.test/path][Bare link]]."))
           (document (parse-org source))
           (links (export-ascii-test-link-nodes document))
           (output (export-org document :ascii))
           (note "[Bare link] <https://bare.test/path>")
           (note-position (search note output))
           (note-second-position
             (and note-position
                  (search note output :start2 (1+ note-position)))))
      (with-soft-assertions
        (expect (length links) :to-be 2)
        (expect (search "[Bare link] and [Bare link]." output)
                :not :to-be nil)
        (expect note-position :not :to-be nil)
        (expect note-second-position :to-be nil))))

  (it "rejects multiline link destinations after bracket normalization"
    (let* ((source
             (export-ascii-test-source
              (format nil "[[https://safe.test/path~%continued][Multiline]]")))
           (document (parse-org source))
           (links (export-ascii-test-link-nodes document))
           (link (first links))
           (link-source
             (and link
                  (subseq source (node-begin link) (node-end link))))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (length links) :to-be 1)
        (expect (and link-source
                     (search (string #\Newline) link-source))
                :not :to-be nil)
        (expect (search "[Multiline]" output) :not :to-be nil)
        (expect (search "[Multiline] <" output) :to-be nil))))

  (it "renders unresolved local links as labels without notes"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "[[#not-exported][Anchor]]"))
           (document (parse-org source))
           (links (export-ascii-test-link-nodes document))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (length links) :to-be 1)
        (expect output :to-equal (export-ascii-test-source "[Anchor]")))))

  (it "resolves included custom-id links and collects a root-level link note"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "[[https://root.test/path][Root link]]."
              "#+INCLUDE: \"included/fragment.org\""))
           (document (parse-org source :source-name "root.org"))
           (calls nil)
           (resolver
             (lambda (path parent-source)
               (push (list path parent-source) calls)
               (if (string= path "included/fragment.org")
                   (export-ascii-test-source
                    "* Included target"
                    ":PROPERTIES:"
                    ":CUSTOM_ID: included-target"
                    ":END:"
                    "[[#included-target][Anchor]].")
                   (error "Unexpected ASCII export include: ~A" path))))
           (output
             (export-org document :ascii :include-resolver resolver))
           (root-note "[Root link] <https://root.test/path>")
           (root-note-position (search root-note output))
           (root-note-second-position
             (and root-note-position
                  (search root-note output :start2 (1+ root-note-position))))
           (included-note "[Anchor] See section Included target")
           (included-note-position (search included-note output))
           (included-note-second-position
             (and included-note-position
                  (search included-note output :start2
                          (1+ included-note-position)))))
      (with-soft-assertions
        (expect (nreverse calls)
                :to-equal '(("included/fragment.org" "root.org")))
        (expect root-note-position :not :to-be nil)
        (expect root-note-second-position :to-be nil)
        (expect included-note-position :not :to-be nil)
        (expect included-note-second-position :to-be nil))))

  (it "renders a root-level described URL link and its note"
    (let* ((source
             (export-ascii-test-source "[[https://bare.test][Bare link]]"))
           (output (export-org (parse-org source) :ascii))
           (expected
             (export-ascii-test-source
              "[Bare link]"
              ""
              ""
              "[Bare link] <https://bare.test>")))
      (expect output :to-equal expected)))

  (it "renders radio links without destination notes"
    (let* ((source
             (export-ascii-test-source "<<<radio target>>> radio target."))
           (document (parse-org source))
           (link (export-ascii-test-find-node (document-root document) :link))
           (output (export-org document :ascii)))
      (with-soft-assertions
        (expect (and link (node-property link :type)) :to-equal "radio")
        (expect output :to-equal (export-ascii-test-source "radio target."))
        (expect (search "[radio target] <" output) :to-be nil))))

  (it "uses the shared unsupported-renderer fallback"
    (let* ((preparation
             (org-kit::export-prepare (parse-org "Body") :ascii))
           (condition
             (export-ascii-test-catch-error
              (lambda ()
                (org-kit::export-render-preparation preparation :unregistered)))))
      (with-soft-assertions
        (expect (typep condition 'org-export-error) :to-be t)
        (when (typep condition 'org-export-error)
          (expect (org-export-error-backend condition) :to-be :unregistered)
          (expect (org-export-error-reason condition) :to-be :unsupported-backend)))))

  (it "keeps Babel disabled while exporting source text"
    (let* ((source
             (export-ascii-test-source
              "#+BEGIN_SRC emacs-lisp :eval yes"
              "(error \"Babel must remain disabled\")"
              "#+END_SRC"))
           (output (export-org (parse-org source) :ascii)))
      (with-soft-assertions
        (expect (search "Babel must remain disabled" output)
                :not :to-be nil))))

  (it "renders anonymous inline footnotes at their reference point"
    (let* ((source
             (export-ascii-test-source "Before [fn::inline note] after."))
           (output (export-org (parse-org source) :ascii)))
      (expect output
              :to-equal (export-ascii-test-source "Before inline note after."))))

  (it "signals shared preparation errors for unsupported syntax and footnotes"
    (let* ((diary
             (export-ascii-test-catch-error
              (lambda ()
                (export-org (parse-org "%%(org-calendar-holiday)\n") :ascii))))
           (footnote
             (export-ascii-test-catch-error
              (lambda ()
                (export-org (parse-org "Missing [fn:missing].\n") :ascii)))))
      (with-soft-assertions
        (expect (typep diary 'org-export-unsupported-syntax) :to-be t)
        (when (typep diary 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-backend diary) :to-be :ascii)
          (expect (org-export-unsupported-syntax-feature diary)
                  :to-be :diary-sexp))
        (expect (typep footnote 'org-export-error) :to-be t)
        (when (typep footnote 'org-export-error)
          (expect (org-export-error-backend footnote) :to-be :ascii)
          (expect (org-export-error-reason footnote) :to-be :unresolved-footnote)))))

  (it "reuses stable output without mutating source nodes"
    (let* ((source
             (export-ascii-test-source
              "#+OPTIONS: toc:nil num:nil"
              "* Heading :tag:"
              "Paragraph with *bold* and /italic/."
              "- item"))
           (document (parse-org source))
           (before (export-ascii-test-document-snapshot document))
           (first-output (export-org document :ascii))
           (second-output (export-org document :ascii)))
      (with-soft-assertions
        (expect first-output :to-equal second-output)
        (expect (export-ascii-test-document-snapshot document) :to-equal before)))))
