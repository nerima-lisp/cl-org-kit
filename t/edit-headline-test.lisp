;;;; t/edit-headline-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-headline-test-nodes (document)
  (let ((headlines '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(defun edit-headline-test-source-parts-match-p
    (source edited start end replacement)
  (let* ((replacement-end (+ start (length replacement)))
         (suffix-length (- (length source) end)))
    (and (<= 0 start end (length source))
         (= (length edited) (+ replacement-end suffix-length))
         (string= source edited :start1 0 :end1 start
                                :start2 0 :end2 start)
         (string= replacement edited :start1 0 :end1 (length replacement)
                                    :start2 start :end2 replacement-end)
         (string= source edited :start1 end :end1 (length source)
                                :start2 replacement-end :end2 (length edited)))))

(defun edit-headline-test-patched-octets (source start end replacement)
  (let* ((replacement-end (+ start (length replacement)))
         (result (make-array (+ start (length replacement) (- (length source) end))
                             :element-type '(unsigned-byte 8))))
    (replace result source :start1 0 :end1 start :start2 0 :end2 start)
    (replace result replacement :start1 start :start2 0)
    (replace result source :start1 replacement-end :start2 end)
    result))

(defun edit-headline-test-octet-edit
    (document target operation pattern replacement)
  (let* ((source (write-org document :output-type :octets))
         (start (search pattern source)))
    (unless start
      (error "Expected octet patch pattern was absent"))
    (let* ((end (+ start (length pattern)))
           (expected (edit-headline-test-patched-octets
                      source start end replacement))
           (edited (funcall operation document target)))
      (with-soft-assertions
        (expect (write-org document :output-type :octets) :to-equalp source)
        (expect (write-org edited :output-type :octets) :to-equalp expected))
      edited)))

(defun edit-headline-test-only-properties-changed-p
    (original-tree edited path changed-properties)
  (let ((before (org-kit::edit-canonical-copy original-tree))
        (after (org-kit::edit-canonical-document edited)))
    (dolist (property changed-properties)
      (org-kit::edit-canonical-remove-property before path property)
      (org-kit::edit-canonical-remove-property after path property))
    (org-kit::edit-canonical-equal-p before after)))

(defun edit-headline-test-assert-result
    (document edited source original-tree path start end replacement
              changed-properties)
  (with-soft-assertions
    (expect (eq edited document) :to-be nil)
    (expect (document-text document) :to-equal source)
    (expect (org-kit::edit-canonical-equal-p
             (org-kit::edit-canonical-document document) original-tree)
            :to-be t)
    (expect (edit-headline-test-source-parts-match-p
             (document-text document) (document-text edited)
             start end replacement)
            :to-be t)
    (expect (edit-headline-test-only-properties-changed-p
             original-tree edited path changed-properties)
            :to-be t)))

(defun edit-headline-test-expect-invalid-value (document target operation)
  (let ((original-text (document-text document))
        (original-tree (org-kit::edit-canonical-document document)))
    (handler-case
        (progn
          (funcall operation)
          (expect nil :to-be t))
      (org-edit-error (condition)
        (with-soft-assertions
          (expect (org-edit-error-target condition) :to-equal "headline")
          (expect (org-edit-error-reason condition) :to-be :invalid-value))))
    (with-soft-assertions
      (expect (document-text document) :to-equal original-text)
      (expect (org-kit::edit-canonical-equal-p
               (org-kit::edit-canonical-document document) original-tree)
              :to-be t))))

(describe "headline edits"
  (it "changes only the selected TODO field and preserves every other source range"
    (let* ((source (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task :old:~%* Peer~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (todo-start (search "TODO" source :start2 (node-begin headline)))
           (edited (set-heading-todo document headline "DONE"))
           (edited-headline (first (edit-headline-test-nodes edited))))
      (edit-headline-test-assert-result
       document edited source original-tree path todo-start (+ todo-start 4) "DONE"
       '(:todo-keyword :todo-type))
      (with-soft-assertions
        (expect (node-property edited-headline :todo-keyword) :to-equal "DONE")
        (expect (node-property edited-headline :todo-type) :to-be :done))))

  (it "changes only the selected priority field"
    (let* ((source (format nil "* TODO [#B] Task :old:~%* Peer~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (priority-start (search "[#B]" source :start2 (node-begin headline)))
           (edited (set-heading-priority document headline #\A))
           (edited-headline (first (edit-headline-test-nodes edited))))
      (edit-headline-test-assert-result
       document edited source original-tree path
       priority-start (+ priority-start 4) "[#A]"
       '(:priority))
      (expect (node-property edited-headline :priority) :to-be (char-code #\A))))

  (it "accepts a one-character string priority through the public API"
    (let* ((source (format nil "* [#B] Task~%* Peer~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (priority-start (search "[#B]" source :start2 (node-begin headline)))
           (edited (set-heading-priority document headline "C"))
           (edited-headline (first (edit-headline-test-nodes edited))))
      (edit-headline-test-assert-result
       document edited source original-tree path
       priority-start (+ priority-start 4) "[#C]" '(:priority))
      (expect (node-property edited-headline :priority) :to-equal (char-code #\C))))

  (it "sets a digit priority using the parsed numeric value"
    (let* ((source (format nil "* Task~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (edited (set-heading-priority document headline #\7)))
      (with-soft-assertions
        (expect (document-text edited) :to-equal (format nil "* [#7] Task~%"))
        (expect (node-property (first (edit-headline-test-nodes edited)) :priority)
                :to-equal 7))))

  (it "replaces the raw title while retaining inline objects and tags"
    (let* ((source (format nil "* TODO [#B] Old *bold* :old:~%* Peer~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (title-start (search "Old *bold*" source :start2 (node-begin headline)))
           (replacement "New /italic/")
           (edited (replace-heading-title document headline replacement))
           (edited-headline (first (edit-headline-test-nodes edited))))
      (edit-headline-test-assert-result
       document edited source original-tree path title-start
       (+ title-start (length "Old *bold*"))
       replacement '(:raw-value :title))
      (with-soft-assertions
        (expect (node-property edited-headline :raw-value) :to-equal replacement)
        (expect (node-property edited-headline :tags) :to-equal '("old")))))

  (it "separates a newly inserted title from an existing tag suffix"
    (let* ((source (format nil "* :keep:~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (title-start (+ (node-begin headline) 2))
           (edited (replace-heading-title document headline "Title")))
      (edit-headline-test-assert-result
       document edited source original-tree path title-start title-start "Title "
       '(:raw-value :title))
      (expect (node-property (first (edit-headline-test-nodes edited)) :tags)
              :to-equal '("keep"))))

  (it "appends new tags in order and retains existing tags"
    (let* ((source (format nil "* Task :old:~%* Peer :peer:~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (tag-start (search ":old:" source :start2 (node-begin headline)))
           (edited (add-heading-tags document headline '("new" "work")))
           (edited-headline (first (edit-headline-test-nodes edited))))
      (edit-headline-test-assert-result
       document edited source original-tree path tag-start
       (+ tag-start (length ":old:"))
       ":old:new:work:" '(:tags :archivedp))
      (expect (node-property edited-headline :tags)
              :to-equal '("old" "new" "work"))))

  (it "removes only named tags and updates archive state"
    (let* ((source (format nil "* Task :keep:drop:ARCHIVE:~%* Peer~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (tag-start (search ":keep:drop:ARCHIVE:" source
                              :start2 (node-begin headline)))
           (edited (remove-heading-tags document headline '("drop" "ARCHIVE")))
           (edited-headline (first (edit-headline-test-nodes edited))))
      (edit-headline-test-assert-result
       document edited source original-tree path tag-start
       (+ tag-start (length ":keep:drop:ARCHIVE:")) ":keep:"
       '(:tags :archivedp))
      (with-soft-assertions
        (expect (node-property edited-headline :tags) :to-equal '("keep"))
        (expect (node-property edited-headline :archivedp) :to-be nil))))

  (it "replaces the tag list in the supplied order"
    (let* ((source (format nil "* Task :old:ARCHIVE:~%* Peer :peer:~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (tag-start (search ":old:ARCHIVE:" source
                              :start2 (node-begin headline)))
           (edited (replace-heading-tags document headline '("work" "new")))
           (edited-headline (first (edit-headline-test-nodes edited))))
      (edit-headline-test-assert-result
       document edited source original-tree path tag-start
       (+ tag-start (length ":old:ARCHIVE:")) ":work:new:"
       '(:tags :archivedp))
      (expect (node-property edited-headline :tags)
              :to-equal '("work" "new"))))

  (it "removes every existing tag when the replacement list is empty"
    (let* ((source (format nil "* Task :old:ARCHIVE:~%* Peer :peer:~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (tag-start (search ":old:ARCHIVE:" source
                              :start2 (node-begin headline)))
           (tag-end (+ tag-start (length ":old:ARCHIVE:")))
           (edited (replace-heading-tags document headline nil))
           (edited-headline (first (edit-headline-test-nodes edited))))
      (edit-headline-test-assert-result
       document edited source original-tree path tag-start tag-end ""
       '(:tags :archivedp))
      (with-soft-assertions
        (expect (document-text edited) :to-equal (format nil "* Task ~%* Peer :peer:~%"))
        (expect (node-property edited-headline :tags) :to-equal nil)
        (expect (node-property edited-headline :archivedp) :to-be nil))))

  (it "inserts a new tag suffix when the headline has no tags"
    (let* ((source (format nil "* Plain~%* Peer~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (tag-position (+ (node-begin headline) (length "* Plain")))
           (edited (add-heading-tags document headline '("new"))))
      (edit-headline-test-assert-result
       document edited source original-tree path tag-position tag-position " :new:"
       '(:tags :archivedp))
      (expect (node-property (first (edit-headline-test-nodes edited)) :tags)
              :to-equal '("new"))))

  (it "adds TODO and priority tokens to headlines that do not have them"
    (let* ((source (format nil "* Task~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (todo-position (1+ (node-begin headline)))
           (after-todo (set-heading-todo document headline "TODO"))
           (after-todo-source (document-text after-todo))
           (after-todo-tree (org-kit::edit-canonical-document after-todo))
           (after-todo-headline (first (edit-headline-test-nodes after-todo)))
           (after-todo-path (org-kit::edit-node-path after-todo after-todo-headline))
           (priority-position
             (+ (node-begin after-todo-headline) (length "* TODO")))
           (after-priority
             (set-heading-priority after-todo after-todo-headline #\C)))
      (edit-headline-test-assert-result
       document after-todo source original-tree path todo-position todo-position
       " TODO" '(:todo-keyword :todo-type))
      (edit-headline-test-assert-result
       after-todo after-priority after-todo-source after-todo-tree after-todo-path
       priority-position priority-position " [#C]" '(:priority))))

  (it "clears TODO and priority without changing the remaining headline"
    (let* ((source (format nil "* TODO [#B] Task :old:~%* Peer~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (todo-start (search "TODO" source :start2 (node-begin headline)))
           (without-todo (set-heading-todo document headline nil))
           (without-todo-source (document-text without-todo))
           (without-todo-tree (org-kit::edit-canonical-document without-todo))
           (without-todo-headline (first (edit-headline-test-nodes without-todo)))
           (without-todo-path (org-kit::edit-node-path without-todo
                                                       without-todo-headline))
           (priority-start (search "[#B]" (document-text without-todo)
                                   :start2 (node-begin without-todo-headline)))
           (without-priority
             (set-heading-priority without-todo without-todo-headline nil)))
      (edit-headline-test-assert-result
       document without-todo source original-tree path
       todo-start (+ todo-start 4) ""
       '(:todo-keyword :todo-type))
      (edit-headline-test-assert-result
       without-todo without-priority without-todo-source without-todo-tree
       without-todo-path
       priority-start (+ priority-start 4) "" '(:priority))
      (expect (node-property (first (edit-headline-test-nodes without-priority))
                             :todo-keyword)
              :to-be nil)))

  (it "reports invalid values with bounded target and reason slots"
    (let* ((document (parse-org (format nil "* Task~%")))
           (headline (first (edit-headline-test-nodes document))))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (set-heading-todo document headline "UNKNOWN")))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (set-heading-priority document headline "AB")))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda ()
         (replace-heading-title document headline
                                (format nil "line one~%line two"))))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (add-heading-tags document headline '("has space"))))))

  (it-property "random headline edit sequences preserve outside text and the input document"
      ((steps (gen-list (gen-integer :min 0 :max 63) :max-length 20)))
    (let* ((newline (string #\Newline))
           (crlf (concatenate 'string (string #\Return) newline))
           (source (concatenate 'string "prefix" newline
                                "* TODO [#C] Seed :seed:tag0:" crlf
                                "middle" newline "* Peer :peer:" newline
                                "suffix"))
           (original (parse-org source))
           (original-tree (org-kit::edit-canonical-document original))
           (document original)
           (headline (first (edit-headline-test-nodes document)))
           (line-start (node-begin headline))
           (after-line (1+ (position #\Newline source :start line-start)))
           (prefix (subseq source 0 line-start))
           (suffix (subseq source after-line)))
      (dolist (step (cons 0 steps))
        (let* ((before document)
               (before-tree (org-kit::edit-canonical-document document))
               (before-path (org-kit::edit-node-path document headline))
               (operation (mod step 8))
               (changed-properties
                 (case operation
                   ((0 1 7) '(:todo-keyword :todo-type))
                   (2 '(:priority))
                   (3 '(:raw-value :title))
                   ((4 5 6) '(:tags :archivedp))))
               (edited
                 (case operation
                   (0 (set-heading-todo document headline "TODO"))
                   (1 (set-heading-todo document headline "DONE"))
                   (2 (set-heading-priority
                       document headline (nth (mod step 4) '(#\A #\B #\C #\D))))
                   (3 (replace-heading-title
                       document headline (format nil "title-~D" step)))
                   (4 (add-heading-tags
                       document headline (list (format nil "tag~D" (mod step 5)))))
                   (5 (remove-heading-tags
                       document headline (list (format nil "tag~D" (mod step 5)))))
                   (7 (set-heading-todo document headline nil))
                   (6 (replace-heading-tags
                       document headline
                       (list "work" (format nil "tag~D" (mod step 5)))))))
               (next-headline (first (edit-headline-test-nodes edited)))
               (text (document-text edited)))
          (with-soft-assertions
            (expect (string= text prefix :start1 0 :end1 (length prefix)
                                       :start2 0 :end2 (length prefix))
                    :to-be t)
            (expect (string= text suffix
                             :start1 (- (length text) (length suffix))
                             :end1 (length text)
                             :start2 0 :end2 (length suffix))
                    :to-be t)
            (expect (document-text original) :to-equal source)
            (expect (org-kit::edit-canonical-equal-p
                     (org-kit::edit-canonical-document original) original-tree)
                    :to-be t)
            (expect (edit-headline-test-only-properties-changed-p
                     before-tree edited before-path changed-properties)
                    :to-be t))
          (setf document edited
                headline next-headline)))))

  (it "preserves exact UTF-8 octets around edits with a BOM, CRLF, and non-ASCII text"
    (let* ((source (octets
                    #xEF #xBB #xBF
                    #xE5 #x89 #x8D #xE6 #x96 #x87 #x0D #x0A
                    #x2A #x20 #x54 #x4F #x44 #x4F #x20 #x5B #x23 #x42 #x5D
                    #x20 #xE4 #xBD #x9C #xE6 #xA5 #xAD #x20
                    #x3A #x6F #x6C #x64 #x3A #x0D #x0A
                    #xE5 #xBE #x8C #xE6 #x96 #x87))
           (document (parse-org source :encoding :utf-8))
           (headline (first (edit-headline-test-nodes document)))
           (after-todo
             (edit-headline-test-octet-edit
              document headline
              (lambda (input node) (set-heading-todo input node "DONE"))
              (octets #x54 #x4F #x44 #x4F)
              (octets #x44 #x4F #x4E #x45)))
           (after-todo-headline
             (first (edit-headline-test-nodes after-todo)))
           (after-priority
             (edit-headline-test-octet-edit
              after-todo after-todo-headline
              (lambda (input node) (set-heading-priority input node #\A))
              (octets #x5B #x23 #x42 #x5D)
              (octets #x5B #x23 #x41 #x5D)))
           (after-priority-headline
             (first (edit-headline-test-nodes after-priority)))
           (after-title
             (edit-headline-test-octet-edit
              after-priority after-priority-headline
              (lambda (input node) (replace-heading-title input node "仕事"))
              (octets #xE4 #xBD #x9C #xE6 #xA5 #xAD)
              (octets #xE4 #xBB #x95 #xE4 #xBA #x8B)))
           (after-title-headline
             (first (edit-headline-test-nodes after-title)))
           (after-add
             (edit-headline-test-octet-edit
              after-title after-title-headline
              (lambda (input node) (add-heading-tags input node '("new")))
              (octets #x3A #x6F #x6C #x64 #x3A)
              (octets #x3A #x6F #x6C #x64 #x3A
                      #x6E #x65 #x77 #x3A)))
           (after-add-headline (first (edit-headline-test-nodes after-add)))
           (after-remove
             (edit-headline-test-octet-edit
              after-add after-add-headline
              (lambda (input node) (remove-heading-tags input node '("old")))
              (octets #x3A #x6F #x6C #x64 #x3A #x6E #x65 #x77 #x3A)
              (octets #x3A #x6E #x65 #x77 #x3A)))
           (after-remove-headline
             (first (edit-headline-test-nodes after-remove)))
           (after-replace
             (edit-headline-test-octet-edit
              after-remove after-remove-headline
              (lambda (input node) (replace-heading-tags input node '("final")))
              (octets #x3A #x6E #x65 #x77 #x3A)
              (octets #x3A #x66 #x69 #x6E #x61 #x6C #x3A))))
      (expect (document-bom-p after-replace) :to-be t)
      (expect (document-encoding after-replace) :to-be :utf-8))))
