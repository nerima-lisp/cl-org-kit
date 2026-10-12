;;;; t/edit-subtree-test.lisp
(in-package #:cl-org-kit/test)

(defun edit-subtree-test-headlines (document)
  (let ((headlines '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(defun edit-subtree-test-headline (document title)
  (find title (edit-subtree-test-headlines document)
        :key (lambda (node) (node-property node :raw-value))
        :test #'string=))

(defun edit-subtree-test-splice (source start end replacement)
  (concatenate 'string (subseq source 0 start) replacement (subseq source end)))

(defun edit-subtree-test-canonical (document)
  (org-kit::edit-canonical-document document))

(defun edit-subtree-test-insert-canonical (canonical path index child)
  (let* ((parent (org-kit::edit-canonical-node-at-path canonical path))
         (children (getf parent :children)))
    (setf (getf parent :children)
          (append (subseq children 0 index) (list child)
                  (nthcdr index children)))
    canonical))

(defun edit-subtree-test-delete-canonical (canonical path)
  (let* ((parent-path (butlast path))
         (index (car (last path)))
         (parent (org-kit::edit-canonical-node-at-path canonical parent-path))
         (children (getf parent :children)))
    (setf (getf parent :children)
          (append (subseq children 0 index) (nthcdr (1+ index) children)))
    canonical))

(defun edit-subtree-test-replace-canonical (canonical path child)
  (let* ((parent-path (butlast path))
         (index (car (last path)))
         (parent (org-kit::edit-canonical-node-at-path canonical parent-path))
         (children (getf parent :children)))
    (setf (getf parent :children)
          (append (subseq children 0 index) (list child)
                  (nthcdr (1+ index) children)))
    canonical))

(defun edit-subtree-test-fragment-canonical (raw)
  (first (getf (edit-subtree-test-canonical (parse-org raw)) :children)))

(defun edit-subtree-test-utf8-bom-octets (source)
  (cl-codec-kit:string-to-octets
   (concatenate 'string (string (code-char #xfeff)) source)
   :encoding :utf-8))

(defun edit-subtree-test-expect-octets (document source)
  (with-soft-assertions
    (expect (document-bom-p document) :to-be t)
    (expect (write-org document :output-type :octets)
            :to-equalp (edit-subtree-test-utf8-bom-octets source))))

(defun edit-subtree-test-assert-edit (input edited source expected-source expected-tree)
  (with-soft-assertions
    (expect (document-text input) :to-equal source)
    (expect (document-text edited) :to-equal expected-source)
    (expect (edit-subtree-test-canonical edited) :to-equalp expected-tree)
    (expect (edit-subtree-test-canonical (parse-org expected-source))
            :to-equalp expected-tree)))

(defun edit-subtree-test-assert-error (thunk reason document source)
  (handler-case
      (progn
        (funcall thunk)
        (expect nil :to-be t))
    (org-edit-error (condition)
      (with-soft-assertions
        (expect (org-edit-error-reason condition) :to-be reason)
        (expect (stringp (org-edit-error-target condition)) :to-be t)
        (expect (<= (length (org-edit-error-target condition)) 80) :to-be t)
        (expect (document-text document) :to-equal source)))))

(describe "headline subtree edits"
  (it "inserts a last child and changes only the requested canonical subtree"
    (let* ((source (format nil "* Parent~%Body~%* Peer~%Peer body~%"))
           (document (parse-org source))
           (parent (edit-subtree-test-headline document "Parent"))
           (peer (edit-subtree-test-headline document "Peer"))
           (peer-before (org-kit::edit-canonical-node peer))
           (raw "** Child~%Child body~%")
           (offset (search "* Peer" source))
           (expected-source (edit-subtree-test-splice source offset offset raw))
           (expected-tree
             (edit-subtree-test-insert-canonical
              (edit-subtree-test-canonical document)
              (org-kit::edit-node-path document parent)
              (length (getf (org-kit::edit-canonical-node parent) :children))
              (edit-subtree-test-fragment-canonical raw)))
           (edited (insert-subtree document parent raw)))
      (edit-subtree-test-assert-edit document edited source expected-source expected-tree)
      (with-soft-assertions
        (expect (node-property (edit-subtree-test-headline edited "Child") :level)
                :to-equal 2)
        (expect (org-kit::edit-canonical-node
                 (edit-subtree-test-headline edited "Peer"))
                :to-equalp peer-before))))

  (it "inserts a first child before existing children"
    (let* ((source (format nil "* Parent~%** Old~%* Peer~%"))
           (document (parse-org source))
           (parent (edit-subtree-test-headline document "Parent"))
           (raw (format nil "** New~%"))
           (offset (search "** Old" source))
           (expected-source (edit-subtree-test-splice source offset offset raw))
           (parent-path (org-kit::edit-node-path document parent))
           (old-children (getf (org-kit::edit-canonical-node parent) :children))
           (index (org-kit::edit-subtree-headline-child-index old-children))
           (expected-tree
             (edit-subtree-test-insert-canonical
              (edit-subtree-test-canonical document) parent-path index
              (edit-subtree-test-fragment-canonical raw)))
           (edited (insert-subtree document parent raw :position :first-child)))
      (edit-subtree-test-assert-edit document edited source expected-source expected-tree)
      (with-soft-assertions
        (expect (node-property (edit-subtree-test-headline edited "New") :level)
                :to-equal 2)
        (expect (node-property (edit-subtree-test-headline edited "Old") :level)
                :to-equal 2))))

  (it "inserts the first child when the parent has no headline children"
    (let* ((source (format nil "* Parent~%* Peer~%"))
           (document (parse-org source))
           (parent (edit-subtree-test-headline document "Parent"))
           (raw (format nil "** Child~%"))
           (offset (search "* Peer" source))
           (expected-source (edit-subtree-test-splice source offset offset raw))
           (expected-tree
             (edit-subtree-test-insert-canonical
              (edit-subtree-test-canonical document)
              (org-kit::edit-node-path document parent) 0
              (edit-subtree-test-fragment-canonical raw)))
           (edited (insert-subtree document parent raw :position :first-child)))
      (edit-subtree-test-assert-edit document edited source expected-source
                                     expected-tree)))

  (it "rejects a child fragment whose logical level is not below its parent"
    (let* ((source (format nil "* Parent~%"))
           (document (parse-org source))
           (parent (edit-subtree-test-headline document "Parent")))
      (edit-subtree-test-assert-error
       (lambda () (insert-subtree document parent (format nil "*** Child~%")))
       :invalid-value document source)))

  (it "deletes one subtree while retaining the following source and tree"
    (let* ((source (format nil "* One~%Body one~%* Two~%Body two~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "One"))
           (start (node-begin target))
           (end (search "* Two" source))
           (expected-source (edit-subtree-test-splice source start end ""))
           (expected-tree
             (edit-subtree-test-delete-canonical
              (edit-subtree-test-canonical document)
              (org-kit::edit-node-path document target)))
           (edited (delete-subtree document target)))
      (edit-subtree-test-assert-edit document edited source expected-source expected-tree)
      (with-soft-assertions
        (expect (mapcar (lambda (node) (node-property node :raw-value))
                        (edit-subtree-test-headlines edited))
                :to-equal '("Two")))))

  (it "deletes the final subtree through the document end"
    (let* ((source (format nil "* Keep~%* Remove~%body~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Remove"))
           (start (node-begin target))
           (expected-source (edit-subtree-test-splice
                             source start (length source) ""))
           (expected-tree
             (edit-subtree-test-delete-canonical
              (edit-subtree-test-canonical document)
              (org-kit::edit-node-path document target)))
           (edited (delete-subtree document target)))
      (edit-subtree-test-assert-edit document edited source expected-source
                                     expected-tree)))

  (it "replaces one subtree and leaves its peer untouched"
    (let* ((source (format nil "* Old~%Old body~%* Peer~%Peer body~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Old"))
           (peer-before (org-kit::edit-canonical-node
                         (edit-subtree-test-headline document "Peer")))
           (raw (format nil "* New~%New body~%"))
           (start (node-begin target))
           (end (search "* Peer" source))
           (expected-source (edit-subtree-test-splice source start end raw))
           (expected-tree
             (edit-subtree-test-replace-canonical
              (edit-subtree-test-canonical document)
              (org-kit::edit-node-path document target)
              (edit-subtree-test-fragment-canonical raw)))
           (edited (replace-subtree document target raw)))
      (edit-subtree-test-assert-edit document edited source expected-source expected-tree)
      (expect (org-kit::edit-canonical-node
               (edit-subtree-test-headline edited "Peer"))
              :to-equalp peer-before)))

  (it "uses the following CRLF when replacing the first subtree"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string "* Old" crlf "* Peer" crlf))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Old"))
           (raw (concatenate 'string "* New" crlf))
           (expected-source (concatenate 'string "* New" crlf "* Peer" crlf))
           (expected-tree
             (edit-subtree-test-replace-canonical
              (edit-subtree-test-canonical document)
              (org-kit::edit-node-path document target)
              (edit-subtree-test-fragment-canonical raw)))
           (edited (replace-subtree document target raw)))
      (edit-subtree-test-assert-edit document edited source expected-source
                                     expected-tree)))

  (it "rejects replacement fragments without exactly one top-level headline"
    (let* ((source (format nil "* Old~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Old")))
      (edit-subtree-test-assert-error
       (lambda () (replace-subtree document target "plain text"))
       :invalid-value document source)
      (edit-subtree-test-assert-error
       (lambda () (replace-subtree document target
                                   (format nil "* First~%* Second~%")))
       :invalid-value document source)))

  (it "requires replacement headlines to preserve logical and source levels"
    (let* ((source (format nil "* Keep~%** Old~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Old")))
      (edit-subtree-test-assert-error
       (lambda () (replace-subtree document target (format nil "* New~%")))
       :invalid-value document source))
    (let* ((source (format nil "#+STARTUP: odd~%*** Old~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Old")))
      (edit-subtree-test-assert-error
       (lambda () (replace-subtree document target (format nil "** New~%")))
       :invalid-value document source)))

  (it "preserves a missing final newline when replacing the final subtree"
    (let* ((source "* Keep\n* Old")
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Old"))
           (raw "* New")
           (start (node-begin target))
           (expected-source (edit-subtree-test-splice source start (length source) raw))
           (expected-tree
             (edit-subtree-test-replace-canonical
              (edit-subtree-test-canonical document)
              (org-kit::edit-node-path document target)
              (edit-subtree-test-fragment-canonical raw)))
           (edited (replace-subtree document target raw)))
      (edit-subtree-test-assert-edit document edited source expected-source expected-tree)
      (expect (document-final-newline-p edited) :to-be nil)))

  (it "inserts at end of a document without adding a final newline"
    (let* ((source "* Parent")
           (document (parse-org source))
           (parent (edit-subtree-test-headline document "Parent"))
           (raw "** Child")
           (expected-source (format nil "* Parent~%** Child"))
           (expected-tree
             (edit-subtree-test-insert-canonical
              (edit-subtree-test-canonical document)
              (org-kit::edit-node-path document parent)
              (length (getf (org-kit::edit-canonical-node parent) :children))
              (edit-subtree-test-fragment-canonical raw)))
           (edited (insert-subtree document parent raw)))
      (edit-subtree-test-assert-edit document edited source expected-source expected-tree)
      (expect (document-final-newline-p edited) :to-be nil)))

  (it "promotes the target and descendants by one odd logical level"
    (let* ((source (format nil "#+STARTUP: odd~%* Root~%*** Child~%***** Grandchild~%* Peer~%"))
           (document (parse-org source))
           (child (edit-subtree-test-headline document "Child"))
           (peer-before (org-kit::edit-canonical-node
                         (edit-subtree-test-headline document "Peer")))
           (edited (promote-subtree document child)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text edited)
                :to-equal (format nil "#+STARTUP: odd~%* Root~%* Child~%*** Grandchild~%* Peer~%"))
        (expect (node-property (edit-subtree-test-headline edited "Child") :level)
                :to-equal 1)
        (expect (node-property (edit-subtree-test-headline edited "Grandchild") :level)
                :to-equal 2)
        (expect (node-type (node-parent (edit-subtree-test-headline edited "Child")))
                :to-be :org-data)
        (expect (node-parent (edit-subtree-test-headline edited "Grandchild"))
                :to-be (edit-subtree-test-headline edited "Child"))
        (expect (org-kit::edit-canonical-node
                 (edit-subtree-test-headline edited "Peer"))
                :to-equalp peer-before))))

  (it "demotes the target and moves it under its previous logical peer"
    (let* ((source (format nil "#+STARTUP: odd~%* Parent~%*** First~%*** Target~%***** Existing~%*** Later~%* Peer~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Target"))
           (peer-before (org-kit::edit-canonical-node
                         (edit-subtree-test-headline document "Peer")))
           (edited (demote-subtree document target)))
      (with-soft-assertions
        (expect (document-text document) :to-equal source)
        (expect (document-text edited)
                :to-equal (format nil "#+STARTUP: odd~%* Parent~%*** First~%***** Target~%******* Existing~%*** Later~%* Peer~%"))
        (expect (node-property (edit-subtree-test-headline edited "Target") :level)
                :to-equal 3)
        (expect (node-property (edit-subtree-test-headline edited "Existing") :level)
                :to-equal 4)
        (expect (node-parent (edit-subtree-test-headline edited "Target"))
                :to-be (edit-subtree-test-headline edited "First"))
        (expect (node-parent (edit-subtree-test-headline edited "Existing"))
                :to-be (edit-subtree-test-headline edited "Target"))
        (expect (org-kit::edit-canonical-node
                 (edit-subtree-test-headline edited "Peer"))
                :to-equalp peer-before))))

  (it "demotes a root-level sibling under its previous root-level peer"
    (let* ((source (format nil "* A~%* B~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "B"))
           (expected-source (format nil "* A~%** B~%"))
           (expected-tree (edit-subtree-test-canonical
                           (parse-org expected-source)))
           (edited (demote-subtree document target)))
      (edit-subtree-test-assert-edit document edited source expected-source
                                     expected-tree)
      (with-soft-assertions
        (expect (node-parent (edit-subtree-test-headline edited "B"))
                :to-be (edit-subtree-test-headline edited "A")))))

  (it "demotes a first root-level headline without inventing a parent"
    (let* ((source (format nil "* A~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "A"))
           (expected-source (format nil "** A~%"))
           (expected-tree (edit-subtree-test-canonical
                           (parse-org expected-source)))
           (edited (demote-subtree document target)))
      (edit-subtree-test-assert-edit document edited source expected-source
                                     expected-tree)
      (expect (node-type (node-parent (edit-subtree-test-headline edited "A")))
              :to-be :org-data)))

  (it "rejects logical level one promotion with the edit slots populated"
    (let* ((source (format nil "* Root~%** Child~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Root")))
      (edit-subtree-test-assert-error
       (lambda () (promote-subtree document target))
       :invalid-value document source)))

  (it "promotes a root-level heading with a skipped parent level"
    (let* ((source (format nil "** Root~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Root"))
           (expected-source (format nil "* Root~%"))
           (expected-tree (edit-subtree-test-canonical
                           (parse-org expected-source)))
           (edited (promote-subtree document target)))
      (edit-subtree-test-assert-edit document edited source expected-source
                                     expected-tree)))

  (it "preserves the heading-depth resource condition on demotion"
    (let* ((source (format nil "#+STARTUP: odd~%* A~%*** B~%***** C~%******* D~%"))
           (document (parse-org source :limits (make-limits :max-heading-depth 4)))
           (target (edit-subtree-test-headline document "B")))
      (handler-case
          (progn
            (demote-subtree document target)
            (expect nil :to-be t))
        (org-resource-limit-error (condition)
          (with-soft-assertions
            (expect (org-kit::org-resource-limit-error-limit-name condition)
                    :to-be :max-heading-depth)
            (expect (org-kit::org-resource-limit-error-limit condition) :to-be 4)
            (expect (org-kit::org-resource-limit-error-actual condition) :to-be 5)
            (expect (document-text document) :to-equal source))))))

  (it "rejects adjacent comments and affiliated ownership at structural boundaries"
    (let* ((comment-sources
             (list (format nil "* Parent~%** Child~%body~%#~%* Sibling~%")
                   (format nil "* Parent~%** Child~%body~%# comment~%~%* Sibling~%")
                   (format nil "* Parent~%** Child~%body~%#+COMMENT: note~%* Sibling~%")
                   (format nil "* Parent~%** Child~%body~%#+BEGIN_COMMENT~%note~%#+END_COMMENT~%* Sibling~%")
                   (format nil "* Parent~%** Child~%body~%#+BEGIN_COMMENT~%note~%  #+END_COMMENT  ~%* Sibling~%")))
           (affiliated-source
             (format nil "* Parent~%** Child~%body~%#+NAME: sibling~%* Sibling~%"))
           (affiliated-document (parse-org affiliated-source))
           (affiliated-target
             (edit-subtree-test-headline affiliated-document "Child")))
      (dolist (comment-source comment-sources)
        (let* ((comment-document (parse-org comment-source))
               (comment-target (edit-subtree-test-headline comment-document "Child")))
          (edit-subtree-test-assert-error
           (lambda () (delete-subtree comment-document comment-target))
           :uneditable-structure comment-document comment-source)))
      (edit-subtree-test-assert-error
       (lambda () (replace-subtree affiliated-document affiliated-target "** New"))
       :uneditable-structure affiliated-document affiliated-source)))

  (it "does not treat similarly prefixed special blocks as comment blocks"
    (let* ((source
             (format nil "* Parent~%** Child~%body~%#+BEGIN_COMMENTARY~%note~%#+END_COMMENTARY~%* Sibling~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Child"))
           (expected-source (format nil "* Parent~%* Sibling~%"))
           (expected-tree (edit-subtree-test-canonical
                           (parse-org expected-source)))
           (edited (delete-subtree document target)))
      (edit-subtree-test-assert-edit document edited source expected-source
                                     expected-tree)))

  (it "does not treat text after an END_COMMENT token as a comment terminator"
    (let* ((source
             (format nil "* Parent~%** Child~%body~%#+END_COMMENT extra~%* Sibling~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Child"))
           (expected-source (format nil "* Parent~%* Sibling~%"))
           (expected-tree (edit-subtree-test-canonical
                           (parse-org expected-source)))
           (edited (delete-subtree document target)))
      (edit-subtree-test-assert-edit document edited source expected-source
                                     expected-tree)))

  (it "rejects first-child insertion across an affiliated keyword"
    (let* ((source (format nil "* Parent~%#+NAME: child~%** Child~%body~%"))
           (document (parse-org source))
           (parent (edit-subtree-test-headline document "Parent")))
      (edit-subtree-test-assert-error
       (lambda () (insert-subtree document parent "** New" :position :first-child))
       :uneditable-structure document source)))

  (it "rejects last-child insertion across a trailing comment"
    (let* ((source (format nil "* Parent~%** Child~%body~%# trailing~%* Sibling~%"))
           (document (parse-org source))
           (parent (edit-subtree-test-headline document "Parent")))
      (edit-subtree-test-assert-error
       (lambda () (insert-subtree document parent "** New"))
       :uneditable-structure document source)))

  (it "rejects ATTR affiliated ownership at a subtree boundary"
    (let ((sources
            (list (format nil "* Parent~%** Child~%body~%#+ATTR_HTML: :width 10~%* Sibling~%")
                  (format nil "* Parent~%** Child~%body~%#+NAME: sibling~%* Sibling~%"))))
      (dolist (source sources)
        (let* ((document (parse-org source))
               (target (edit-subtree-test-headline document "Child")))
          (edit-subtree-test-assert-error
           (lambda () (delete-subtree document target))
           :uneditable-structure document source)))))

  (it "rejects invalid, foreign and wrong-type targets through public edits"
    (let* ((source (format nil "* Heading~%Body~%"))
           (document (parse-org source))
           (other-document (parse-org source))
           (headline (edit-subtree-test-headline document "Heading"))
           (foreign (edit-subtree-test-headline other-document "Heading"))
           (paragraph (find :paragraph
                            (let ((nodes '()))
                              (map-nodes (lambda (node) (push node nodes))
                                         (document-root document))
                              nodes)
                            :key #'node-type)))
      (edit-subtree-test-assert-error
       (lambda () (delete-subtree document paragraph))
       :wrong-node-type document source)
      (edit-subtree-test-assert-error
       (lambda () (delete-subtree document foreign))
       :foreign-node document source)
      (edit-subtree-test-assert-error
       (lambda () (replace-subtree document headline ""))
       :invalid-value document source)
      (edit-subtree-test-assert-error
       (lambda () (insert-subtree document paragraph "** Child"))
       :wrong-node-type document source)
      (edit-subtree-test-assert-error
       (lambda () (insert-subtree document headline "** Child" :position :middle))
       :invalid-value document source)))

  (it "rejects non-documents and non-string subtree text through public edits"
    (let* ((source (format nil "* Parent~%"))
           (document (parse-org source))
           (parent (edit-subtree-test-headline document "Parent")))
      (with-soft-assertions
        (signals type-error (insert-subtree nil nil "** Child"))
        (signals type-error (delete-subtree nil nil))
        (signals type-error (replace-subtree nil nil "* New"))
        (signals type-error (promote-subtree nil nil))
        (signals type-error (demote-subtree nil nil)))
      (edit-subtree-test-assert-error
       (lambda () (insert-subtree document parent 17))
       :invalid-value document source)
      (edit-subtree-test-assert-error
       (lambda () (replace-subtree document parent 17))
       :invalid-value document source)))

  (it "reports nodes removed from their parent's children through a public edit"
    (let* ((source (format nil "* Heading~%"))
           (document (parse-org source))
           (target (edit-subtree-test-headline document "Heading")))
      (setf (node-children (node-parent target)) nil)
      (edit-subtree-test-assert-error
       (lambda () (set-heading-priority document target #\A))
       :uneditable-structure document source)))

  (it "rejects a valid fragment when whole-document semantics would change"
    (let* ((source (format nil "*** Existing~%* Parent~%"))
           (document (parse-org source))
           (parent (edit-subtree-test-headline document "Parent")))
      (edit-subtree-test-assert-error
       (lambda ()
         (insert-subtree document parent
                         (format nil "** New~%#+STARTUP: odd~%")))
       :unexpected-diff document source)))

  (it "preserves exact BOM UTF-8 bytes and CRLF around all five subtree edits"
    (let* ((crlf (concatenate 'string (string #\Return) (string #\Newline)))
           (source (concatenate 'string
                                "* 親" crlf "本文" crlf
                                "** 古" crlf "古文" crlf "*** 孫" crlf
                                "* 相手" crlf))
           (document (parse-org (edit-subtree-test-utf8-bom-octets source)
                                :encoding :utf-8))
           (parent (edit-subtree-test-headline document "親"))
           (old (edit-subtree-test-headline document "古"))
           (insert-raw (concatenate 'string "** 新" crlf "新文" crlf
                                    "*** 新孫" crlf))
           (inserted (insert-subtree document parent insert-raw))
           (inserted-source
             (concatenate 'string
                          "* 親" crlf "本文" crlf
                          "** 古" crlf "古文" crlf "*** 孫" crlf
                          insert-raw "* 相手" crlf))
           (deleted (delete-subtree document old))
           (deleted-source (concatenate 'string "* 親" crlf "本文" crlf
                                        "* 相手" crlf))
           (replace-raw (concatenate 'string "** 交換" crlf "交換文" crlf))
           (replaced (replace-subtree document old replace-raw))
           (replaced-source
             (concatenate 'string "* 親" crlf "本文" crlf
                          replace-raw "* 相手" crlf))
           (promoted (promote-subtree document old))
           (promoted-source (concatenate 'string
                                         "* 親" crlf "本文" crlf
                                         "* 古" crlf "古文" crlf "** 孫" crlf
                                         "* 相手" crlf))
           (inserted-new (edit-subtree-test-headline inserted "新"))
           (demoted (demote-subtree inserted inserted-new))
           (demoted-source (concatenate 'string
                                        "* 親" crlf "本文" crlf
                                        "** 古" crlf "古文" crlf "*** 孫" crlf
                                        "*** 新" crlf "新文" crlf
                                        "**** 新孫" crlf "* 相手" crlf)))
      (edit-subtree-test-expect-octets inserted inserted-source)
      (edit-subtree-test-expect-octets deleted deleted-source)
      (edit-subtree-test-expect-octets replaced replaced-source)
      (edit-subtree-test-expect-octets promoted promoted-source)
      (edit-subtree-test-expect-octets demoted demoted-source))))
