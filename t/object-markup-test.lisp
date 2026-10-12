;;;; t/object-markup-test.lisp
(in-package #:cl-org-kit/test)

(defun markup-test-match (document start end range-start)
  (find-if (lambda (object)
             (and (= (node-begin object) start)
                  (not (eq (node-type object) :plain-text))))
           (org-kit::parse-objects document range-start end :paragraph)))

(describe "markup object recognition"
  (it "recognizes all six delimiter types with their contents spans"
    (let* ((source "*bold* /italic/ _under_ +strike+ ~code~ =verbatim=")
           (document (make-document source))
           (descriptors '((#\* :bold nil)
                          (#\/ :italic nil)
                          (#\_ :underline nil)
                          (#\+ :strike-through nil)
                          (#\~ :code t)
                          (#\= :verbatim t)))
           (cursor 0))
      (with-soft-assertions
        (dolist (descriptor descriptors)
          (destructuring-bind (delimiter type opaque-p) descriptor
            (let* ((start (position delimiter source :start cursor))
                   (close (position delimiter source :start (1+ start)))
                   (match (markup-test-match document start (length source) 0))
                   (post-blank (if (< (1+ close) (length source)) 1 0))
                   (match-end (+ (1+ close) post-blank)))
              (expect (node-p match) :to-be t)
              (expect (node-type match) :to-be type)
              (expect (node-end match) :to-be match-end)
              (expect (node-post-blank match) :to-be post-blank)
              (expect (node-properties match) :to-be nil)
              (expect (null (node-children match)) :to-be opaque-p)
              (expect (node-contents-begin match)
                      :to-be (unless opaque-p (1+ start)))
              (expect (node-contents-end match)
                      :to-be (unless opaque-p close))
              (setf cursor match-end)))))))

  (it "uses Org's opening and closing delimiter boundaries"
    (dolist (case '( ("x*bold*" 1)
                     ("* spaced*" 0)
                     ("*unclosed" 0)
                     ("*word*tail" 0)
                     ("*word*]" 0)))
      (destructuring-bind (source start) case
        (let ((document (make-document source)))
          (expect (markup-test-match document start (length source) 0)
                  :to-be nil))))
    (let* ((comma-source "*word*,")
           (comma-match
             (markup-test-match (make-document comma-source)
                                0 (length comma-source) 0))
           (quoted-source "\"*quoted*\"")
           (quoted-start (search "*quoted*" quoted-source))
           (quoted-match
             (markup-test-match (make-document quoted-source)
                                quoted-start (length quoted-source) 0)))
      (expect (and (node-p comma-match)
                   (eq (node-type comma-match) :bold))
              :to-be t)
      (expect (and (node-p quoted-match)
                   (eq (node-type quoted-match) :bold))
              :to-be t)))

  (it "keeps scanning and post-blank consumption inside the supplied end"
    (let* ((source "*word*   tail")
           (document (make-document source))
           (match (markup-test-match document 0 8 0))
           (no-close (markup-test-match document 0 5 0)))
      (with-soft-assertions
        (expect (node-end match) :to-be 8)
        (expect (node-post-blank match) :to-be 2)
        (expect no-close :to-be nil)))))

(describe "markup range edge branches"
  (it "rejects invalid ranges and whitespace before closing delimiters"
    (let ((document (make-document "*word *")))
      (with-soft-assertions
        (expect (markup-test-match document 0 (length (document-text document)) 0)
                :to-be
                nil)))))
