;;;; t/object-markup-test.lisp
(in-package #:cl-org-kit/test)

(defun markup-test-match (document start end range-start)
  (let ((org-kit::*object-parse-range-start* range-start))
    (org-kit::try-parse-markup-object document start end :paragraph)))

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
              (expect (org-kit::object-match-p match) :to-be t)
              (expect (org-kit::object-match-type match) :to-be type)
              (expect (org-kit::object-match-end match) :to-be match-end)
              (expect (org-kit::object-match-post-blank match) :to-be post-blank)
              (expect (org-kit::object-match-properties match) :to-be nil)
              (expect (org-kit::object-match-opaque-p match) :to-be opaque-p)
              (expect (org-kit::object-match-contents-begin match)
                      :to-be (unless opaque-p (1+ start)))
              (expect (org-kit::object-match-contents-end match)
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
      (expect (and (org-kit::object-match-p comma-match)
                   (eq (org-kit::object-match-type comma-match) :bold))
              :to-be t)
      (expect (and (org-kit::object-match-p quoted-match)
                   (eq (org-kit::object-match-type quoted-match) :bold))
              :to-be t)))

  (it "keeps scanning and post-blank consumption inside the supplied end"
    (let* ((source "*word*   tail")
           (document (make-document source))
           (match (markup-test-match document 0 8 0))
           (no-close (markup-test-match document 0 5 0)))
      (with-soft-assertions
        (expect (org-kit::object-match-end match) :to-be 8)
        (expect (org-kit::object-match-post-blank match) :to-be 2)
        (expect no-close :to-be nil)))))
