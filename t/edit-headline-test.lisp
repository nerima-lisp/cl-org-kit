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

(defun edit-headline-test-octet-insertion
    (document target operation position insertion)
  (let* ((source (write-org document :output-type :octets))
         (expected (edit-headline-test-patched-octets
                    source position position insertion))
         (edited (funcall operation document target)))
    (with-soft-assertions
      (expect (write-org document :output-type :octets) :to-equalp source)
      (expect (write-org edited :output-type :octets) :to-equalp expected))
    edited))

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

(defun edit-headline-test-assert-unchanged (document edited source original-tree)
  (let ((source-octets (write-org document :output-type :octets)))
    (with-soft-assertions
      (expect (document-text document) :to-equal source)
      (expect (write-org document :output-type :octets) :to-equalp source-octets)
      (expect (write-org edited :output-type :octets) :to-equalp source-octets)
      (expect (org-kit::edit-canonical-equal-p
               (org-kit::edit-canonical-document document) original-tree)
              :to-be t)
      (expect (org-kit::edit-canonical-equal-p
               (org-kit::edit-canonical-document edited) original-tree)
              :to-be t))))

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
    (let* ((source (format nil "#+PRIORITIES: 1 10 5~%* Task~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (edited (set-heading-priority document headline 7)))
      (with-soft-assertions
        (expect (document-text edited)
                :to-equal (format nil "#+PRIORITIES: 1 10 5~%* [#7] Task~%"))
        (expect (node-property (first (edit-headline-test-nodes edited)) :priority)
                :to-equal 7))))

  (it "adjusts priorities and starts unset values at configured defaults"
    (let* ((alphabetic-source
             (format nil "#+PRIORITIES: A E C~%* [#C] Letter~%* Number~%"))
           (alphabetic (parse-org alphabetic-source))
           (letter (first (edit-headline-test-nodes alphabetic)))
           (number (second (edit-headline-test-nodes alphabetic)))
           (letter-result (adjust-heading-priority alphabetic letter :up))
           (number-result (adjust-heading-priority alphabetic number :down))
           (numeric-source
             (format nil "#+PRIORITIES: 1 10 5~%* [#5] Number~%"))
           (numeric (parse-org numeric-source))
           (numeric-result
             (adjust-heading-priority numeric
                                      (first (edit-headline-test-nodes numeric))
                                      :up)))
      (with-soft-assertions
        (expect (document-text letter-result)
                :to-equal (format nil "#+PRIORITIES: A E C~%* [#B] Letter~%* Number~%"))
        (expect (document-text number-result)
                :to-equal (format nil "#+PRIORITIES: A E C~%* [#C] Letter~%* [#C] Number~%"))
        (expect (document-text numeric-result)
                :to-equal (format nil "#+PRIORITIES: 1 10 5~%* [#4] Number~%")))))

  (it "starts unset priorities at the configured default in either direction"
    (let* ((source (format nil "* Task~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (up-result (adjust-heading-priority document headline :up))
           (down-result (adjust-heading-priority document headline :down))
           (explicit-nil-result
             (adjust-heading-priority document headline :up :previous-direction nil))
           (numeric-source (format nil "#+PRIORITIES: 1 10 5~%* Task~%"))
           (numeric-document (parse-org numeric-source))
           (numeric-result
             (adjust-heading-priority
              numeric-document (first (edit-headline-test-nodes numeric-document)) :up)))
      (with-soft-assertions
        (expect (document-text up-result)
                :to-equal (format nil "* [#B] Task~%"))
        (expect (document-text down-result)
                :to-equal (format nil "* [#B] Task~%"))
        (expect (document-text explicit-nil-result)
                :to-equal (format nil "* [#B] Task~%"))
        (expect (document-text numeric-result)
                :to-equal (format nil "#+PRIORITIES: 1 10 5~%* [#5] Task~%"))
        (multiple-value-bind (value kind valid-p)
            (org-kit::edit-priority-setting-value "65")
          (expect (list value kind valid-p) :to-equal '(65 :alphabetic t)))
        (multiple-value-bind (value kind valid-p)
            (org-kit::edit-priority-setting-value "5")
          (expect (list value kind valid-p) :to-equal '(5 :numeric t))))))

  (it "clears an out-of-range priority and cycles to the opposite end"
    (let* ((source
             (format nil "#+PRIORITIES: A C B~%* [#A] Highest~%* [#C] Lowest~%"))
           (document (parse-org source))
           (headlines (edit-headline-test-nodes document))
           (up-cleared (adjust-heading-priority document (first headlines) :up))
           (up-cycled
             (adjust-heading-priority
              up-cleared (first (edit-headline-test-nodes up-cleared)) :up
              :previous-direction :up))
           (down-cleared (adjust-heading-priority document (second headlines) :down))
           (down-cycled
             (adjust-heading-priority
              down-cleared (second (edit-headline-test-nodes down-cleared)) :down
              :previous-direction :down)))
      (with-soft-assertions
        (expect (document-text up-cleared)
                :to-equal
                (format nil "#+PRIORITIES: A C B~%* Highest~%* [#C] Lowest~%"))
        (expect (document-text up-cycled)
                :to-equal
                (format nil "#+PRIORITIES: A C B~%* [#C] Highest~%* [#C] Lowest~%"))
        (expect (document-text down-cleared)
                :to-equal
                (format nil "#+PRIORITIES: A C B~%* [#A] Highest~%* Lowest~%"))
        (expect (document-text down-cycled)
                :to-equal
                (format nil "#+PRIORITIES: A C B~%* [#A] Highest~%* [#A] Lowest~%")))))

  (it "rejects priorities outside the configured range and invalid directives"
    (let* ((range-source (format nil "#+PRIORITIES: A E C~%* Task~%"))
           (range-document (parse-org range-source))
           (range-heading (first (edit-headline-test-nodes range-document)))
           (invalid-range
             (handler-case
                 (progn (set-heading-priority range-document range-heading "F") nil)
               (org-edit-error (condition) condition)))
           (directive-source
             (format nil "#+PRIORITIES: 10 1 5~%* Task~%"))
           (directive-document (parse-org directive-source))
           (directive-heading
             (first (edit-headline-test-nodes directive-document)))
           (invalid-directive
             (handler-case
                 (progn
                   (set-heading-priority directive-document directive-heading "5")
                   nil)
               (org-edit-error (condition) condition)))
           (invalid-number
             (handler-case
                 (progn
                   (set-heading-priority range-document range-heading 65)
                   nil)
               (org-edit-error (condition) condition)))
           (invalid-direction
             (handler-case
                 (progn
                   (adjust-heading-priority range-document range-heading :sideways)
                   nil)
               (org-edit-error (condition) condition)))
           (invalid-previous-direction
             (handler-case
                 (progn
                   (adjust-heading-priority range-document range-heading :up
                                            :previous-direction :sideways)
                   nil)
               (org-edit-error (condition) condition)))
           (current-source
             (format nil "#+PRIORITIES: A E C~%* [#F] Task~%"))
           (current-document (parse-org current-source))
           (current-heading (first (edit-headline-test-nodes current-document)))
           (current-result
             (adjust-heading-priority current-document current-heading :up)))
      (with-soft-assertions
        (expect (typep invalid-range 'org-edit-error) :to-be t)
        (when (typep invalid-range 'org-edit-error)
          (expect (org-edit-error-reason invalid-range) :to-be :invalid-value))
        (expect (typep invalid-directive 'org-edit-error) :to-be t)
        (when (typep invalid-directive 'org-edit-error)
          (expect (org-edit-error-reason invalid-directive) :to-be :invalid-value))
        (expect (typep invalid-number 'org-edit-error) :to-be t)
        (when (typep invalid-number 'org-edit-error)
          (expect (org-edit-error-reason invalid-number) :to-be :invalid-value))
        (expect (typep invalid-direction 'org-edit-error) :to-be t)
        (when (typep invalid-direction 'org-edit-error)
          (expect (org-edit-error-reason invalid-direction) :to-be :invalid-value))
        (expect (typep invalid-previous-direction 'org-edit-error) :to-be t)
        (when (typep invalid-previous-direction 'org-edit-error)
          (expect (org-edit-error-reason invalid-previous-direction)
                  :to-be :invalid-value))
        (expect (document-text range-document) :to-equal range-source)
        (expect (document-text directive-document) :to-equal directive-source)
        (progn
          (expect (document-text current-result)
                  :to-equal (format nil "#+PRIORITIES: A E C~%* [#E] Task~%"))
          (expect (document-text current-document) :to-equal current-source)))))
  (it "rejects unsupported current priority values before adjusting"
    (let* ((document (parse-org "* Task"))
           (heading (first (edit-headline-test-nodes document))))
      (setf (node-property heading :priority) :unsupported)
      (signals org-edit-error
        (adjust-heading-priority document heading :up))
      (progn
  (expect (node-property heading :priority) :to-be :unsupported)
  (multiple-value-bind (value kind token valid-p)
      (org-kit::edit-priority-object-value (code-char 66))
    (expect (list value kind token valid-p)
        :to-equal '(66 :alphabetic "B" t))))))

  (it "edits a final headline without a trailing newline and preserves its bytes"
    (let* ((source "* Plain")
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (edited
             (edit-headline-test-octet-edit
              document headline
              (lambda (input node) (replace-heading-title input node "New"))
              (octets #x50 #x6C #x61 #x69 #x6E)
              (octets #x4E #x65 #x77))))
      (with-soft-assertions
        (expect (document-text edited) :to-equal "* New")
        (expect (node-property (first (edit-headline-test-nodes edited))
                              :raw-value)
                :to-equal "New"))))

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

  (it "replaces a commented title after TODO and priority fields"
    (let* ((source (format nil "* TODO [#C] COMMENT Old :keep:~%* Peer~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (original-tree (org-kit::edit-canonical-document document))
           (path (org-kit::edit-node-path document headline))
           (title-start (search "Old" source :start2 (node-begin headline)))
           (replacement "New /italic/")
           (edited (replace-heading-title document headline replacement))
           (edited-headline (first (edit-headline-test-nodes edited))))
      (edit-headline-test-assert-result
       document edited source original-tree path title-start (+ title-start 3)
       replacement '(:raw-value :title))
      (with-soft-assertions
        (expect (node-property edited-headline :commentedp) :to-be t)
        (expect (node-property edited-headline :tags) :to-equal '("keep")))))

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

  (it "adds ARCHIVE through the public tag API and preserves surrounding bytes"
    (let* ((source (format nil "* Task~%* Peer :peer:~%"))
           (document (parse-org source))
           (headline (first (edit-headline-test-nodes document)))
           (source-octets (write-org document :output-type :octets))
           (title-start (search (octets #x54 #x61 #x73 #x6B) source-octets))
           (insert-position (+ title-start 4))
           (edited
             (edit-headline-test-octet-insertion
              document headline
              (lambda (input node)
                (replace-heading-tags input node '("ARCHIVE" "work")))
              insert-position
              (octets #x20 #x3A #x41 #x52 #x43 #x48 #x49 #x56 #x45
                      #x3A #x77 #x6F #x72 #x6B #x3A)))
           (edited-headline (first (edit-headline-test-nodes edited))))
      (with-soft-assertions
        (expect (document-text edited)
                :to-equal (format nil "* Task :ARCHIVE:work:~%* Peer :peer:~%"))
        (expect (node-property edited-headline :tags)
                :to-equal '("ARCHIVE" "work"))
        (expect (node-property edited-headline :archivedp) :to-be t))))

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
       priority-start (+ priority-start 5) "" '(:priority))
      (expect (node-property (first (edit-headline-test-nodes without-priority))
                             :todo-keyword)
              :to-be nil)))

  (it "keeps clearing absent fields and empty tag replacement byte-for-byte stable"
    (let* ((source (format nil "* Plain~%* Peer~%"))
           (document (parse-org source))
           (original-tree (org-kit::edit-canonical-document document))
           (headline (first (edit-headline-test-nodes document)))
           (without-todo (set-heading-todo document headline nil))
           (without-todo-headline
             (first (edit-headline-test-nodes without-todo)))
           (without-priority
             (set-heading-priority without-todo without-todo-headline nil))
           (without-priority-headline
             (first (edit-headline-test-nodes without-priority)))
           (without-tags
             (replace-heading-tags without-priority without-priority-headline nil)))
      (edit-headline-test-assert-unchanged document without-todo source original-tree)
      (edit-headline-test-assert-unchanged without-todo without-priority source
                                            original-tree)
      (edit-headline-test-assert-unchanged without-priority without-tags source
                                            original-tree)))

  (it "reports invalid values with bounded target and reason slots"
    (let* ((document (parse-org (format nil "* Task~%")))
           (headline (first (edit-headline-test-nodes document))))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (set-heading-todo document headline "UNKNOWN")))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (set-heading-todo document headline :todo)))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (set-heading-priority document headline "AB")))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (set-heading-priority document headline :priority)))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda ()
         (replace-heading-title document headline
                                (format nil "line one~%line two"))))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (replace-heading-title document headline 42)))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda ()
         (replace-heading-title document headline "Title :embedded:")))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (add-heading-tags document headline '("has space"))))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (add-heading-tags document headline '(:not-a-string))))
      (edit-headline-test-expect-invalid-value
       document headline
       (lambda () (add-heading-tags document headline '("good" . "tail"))))))

  (it "rejects non-document inputs through the public headline API"
    (signals type-error (set-heading-todo nil nil "TODO")))

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
                       document headline (nth (mod step 3) '(#\A #\B #\C))))
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
