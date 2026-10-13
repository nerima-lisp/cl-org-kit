;;;; t/conformance-test.lisp
(in-package #:cl-org-kit/test)

(defparameter +conformance-fixtures+
  '("00-structure.org"
    "01-greater-elements.org"
    "02-lesser-elements.org"
    "03-objects-text.org"
    "04-objects-links-citations.org"
    "05-objects-inline-table-time.org"
    "06-element-boundaries.org"
    "07-object-boundaries.org"
    "settings/inlinetask-levels.org"
    "settings/link-abbrev-defined.org"
    "settings/link-abbrev-undefined.org"
    "settings/startup-default.org"
    "settings/startup-odd.org"
    "settings/todo-custom.org"
    "settings/todo-default.org"
    "variants/crlf-japanese.org"
    "variants/no-final-newline.org"
    "variants/utf8-bom.org"))

(defun conformance-fixture-path (tree relative)
  (asdf:system-relative-pathname
   "cl-org-kit"
   (concatenate 'string "t/fixtures/" tree "/" relative
                (if (string= tree "expected") ".sexp" ""))))

(defun read-conformance-octets (path)
  (with-open-file (stream path :direction :input
                               :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (octets (make-array length :element-type '(unsigned-byte 8)))
           (read-count (read-sequence octets stream)))
      (unless (= read-count length)
        (error "Could not read all bytes from ~A." path))
      octets)))

(defun read-conformance-tree (path)
  (with-open-file (stream path :direction :input)
    (let* ((*read-eval* nil)
           (eof (gensym "EOF"))
           (tree (read stream nil eof)))
      (when (eq tree eof)
        (error "No expected tree in ~A." path))
      (unless (eq (read stream nil eof) eof)
        (error "More than one expected tree in ~A." path))
      tree)))

(defun conformance-node-form-p (value)
  (and (consp value)
       (keywordp (first value))
       (consp (rest value))
       (listp (second value))
       (evenp (length (second value)))))

(defun conformance-proper-list-p (value)
  (and (listp value)
       (handler-case
           (progn (length value) t)
         (type-error () nil))))

(defun conformance-node-property (node key)
  (case key
    (:begin (node-begin node))
    (:end (node-end node))
    (:contents-begin (node-contents-begin node))
    (:contents-end (node-contents-end node))
    (otherwise (node-property node key))))

(defun assert-conformance-value (actual expected fixture)
  (cond
    ((conformance-node-form-p expected)
     (expect (list fixture (node-p actual)) :to-equal (list fixture t))
     (when (node-p actual)
       (assert-conformance-node actual expected fixture)))
    ((and (conformance-proper-list-p actual)
          (conformance-proper-list-p expected))
     (expect (list fixture (length actual))
             :to-equal (list fixture (length expected)))
     (loop for actual-item in actual
           for expected-item in expected
           do (assert-conformance-value actual-item expected-item fixture)))
    (t
     (expect (list fixture actual) :to-equal (list fixture expected)))))

(defun assert-conformance-child (actual expected fixture)
  (cond
    ((stringp expected)
     (expect (list fixture (node-type actual))
             :to-equal (list fixture :plain-text))
     (expect (list fixture (node-text actual))
             :to-equal (list fixture expected)))
    ((conformance-node-form-p expected)
     (assert-conformance-node actual expected fixture))
    (t
     (expect (list fixture actual) :to-equal (list fixture expected)))))

(defun assert-conformance-node (node expected fixture)
  (expect (list fixture (node-type node))
          :to-equal (list fixture (first expected)))
  (loop for (key value) on (second expected) by #'cddr
        do (let ((actual (conformance-node-property node key)))
             (if (and (eq (node-type node) :table-cell)
                      (eq key :value)
                      (null value))
                 (expect (list fixture actual)
                         :to-equal
                         (list fixture
                               (subseq (document-text (node-document node))
                                       (node-contents-begin node)
                                       (node-contents-end node))))
                 (assert-conformance-value actual value fixture))))
  (let ((children (node-children node))
        (expected-children (cddr expected)))
    (expect (list fixture (length children))
            :to-equal (list fixture (length expected-children)))
    (loop for child in children
          for expected-child in expected-children
          do (assert-conformance-child child expected-child fixture))))

(describe "Org element conformance"
  (it "matches all 18 expected trees and preserves their source bytes"
    (let ((read-count 0)
          (comparison-count 0)
          (round-trip-count 0))
      (with-soft-assertions
        (dolist (relative +conformance-fixtures+)
          (let* ((source-path (conformance-fixture-path "corpus" relative))
                 (expected-path (conformance-fixture-path "expected" relative))
                 (source (read-conformance-octets source-path))
                 (expected (read-conformance-tree expected-path))
                 (document (parse-org source))
                 (root (document-root document)))
            (incf read-count)
            (expect (plusp (length source)) :to-be t)
            (expect (first expected) :to-be :org-data)
            (expect (node-p root) :to-be t)
            (when (node-p root)
              (assert-conformance-node root expected relative)
              (incf comparison-count))
            (expect (equalp source (write-org document :output-type :octets))
                    :to-be t)
            (incf round-trip-count)))
        (expect (length +conformance-fixtures+) :to-be 18)
        (expect read-count :to-be 18)
        (expect comparison-count :to-be 18)
        (expect round-trip-count :to-be 18))
      (format t "conformance fixtures: read=~D compared=~D octet-round-trips=~D~%"
              read-count comparison-count round-trip-count)))

  (it "keeps table formula properties and emitted directives in source order"
    (let* ((source (format nil "| x |~%#+TBLFM: $1=$1+1~%#+TBLFM: $1=$1-1~%"))
           (document (parse-org source))
           (section (first (node-children (document-root document))))
           (table (first (node-children section))))
      (expect (node-property table :tblfm)
              :to-equal '("$1=$1+1" "$1=$1-1"))
      (expect (write-org document) :to-equal source))))
