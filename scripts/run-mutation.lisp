;;;; scripts/run-mutation.lisp
;;;;
;;;; Mutate selected core functions with cl-weave and require every mutant to be
;;;; killed by the specs whose names match the target's filter.
(require :asdf)

(defparameter *script-directory*
  (uiop:pathname-directory-pathname *load-truename*))
(defparameter *project-root*
  (uiop:pathname-parent-directory-pathname *script-directory*))

(asdf:load-asd (merge-pathnames "cl-org-kit.asd" *project-root*))
(asdf:load-system "cl-org-kit/test")

(defpackage #:org-kit/mutation
  (:use #:cl))
(in-package #:org-kit/mutation)

(defparameter *targets*
  '(("src/conditions.lisp" bounded-text "bounded condition text")
    ("src/limits.lisp" check-limit "resource limits")
    ("src/input.lisp" strip-bom "input decoding")
    ("src/document.lisp" offset-line-column "documents")
    ("src/document.lisp" document-newline "documents")
    ("src/nodes.lisp" check-span "node construction")
    ("src/nodes.lisp" node-property "node properties"))
  "Each entry names a source file, a DEFUN in ORG-KIT, and the spec name filter
that must kill its mutants.")

(defun function-form (pathname name)
  "Return the DEFUN form for NAME read from PATHNAME."
  (let ((*package* (find-package :org-kit))
        (*read-eval* nil))
    (with-open-file (stream pathname)
      (or (loop for form = (read stream nil :eof)
                until (eq form :eof)
                when (and (consp form) (eq (first form) 'defun) (eq (second form) name))
                  return form)
          (error "No DEFUN ~S in ~A" name pathname)))))

(defun selected-test-count (name-filter)
  (length (cl-weave:collect-test-plan (cl-weave:root-suite) :name-filter name-filter)))

(defun run-selected-tests (name-filter)
  (cl-weave:run-all :reporter :spec :stream (make-broadcast-stream)
                    :name-filter name-filter :max-workers 1 :pass-with-no-tests nil))

(defun eval-definition (form)
  (handler-bind ((style-warning #'muffle-warning))
    (eval form)))

(defun mutation-results (name form name-filter)
  (let ((results
          (cl-weave:run-mutations
           form
           (lambda (mutated mutation)
             (declare (ignore mutation))
             (unwind-protect
                  (progn (eval-definition mutated)
                         (run-selected-tests name-filter))
               (eval-definition form)))
           :timeout-ms 5000)))
    (format t "~A tests=~D ~S~%" name (selected-test-count name-filter)
            (cl-weave:mutation-summary results))
    (dolist (result results)
      (when (member (cl-weave:mutation-result-status result) '(:survived :errored))
        (format t "  ~A operator=~A path=~S~%"
                (cl-weave:mutation-result-status result)
                (cl-weave:mutation-operator (cl-weave:mutation-result-mutation result))
                (cl-weave:mutation-path (cl-weave:mutation-result-mutation result)))))
    results))

(defun main ()
  (let ((all-results '())
        (empty-targets 0))
    (loop for (file name filter) in *targets*
          for symbol = (intern (symbol-name name) :org-kit)
          for results = (mutation-results
                         (symbol-name name)
                         (function-form (merge-pathnames file cl-user::*project-root*) symbol)
                         filter)
          ;; A target selecting no spec, or yielding no mutant, measures nothing.
          do (when (or (zerop (selected-test-count filter)) (null results))
               (incf empty-targets))
             (setf all-results (append results all-results)))
    (let ((survived (count :survived all-results :key #'cl-weave:mutation-result-status))
          (errored (count :errored all-results :key #'cl-weave:mutation-result-status)))
      (format t "mutation-total targets=~D mutants=~D killed=~D survived=~D errored=~D~%"
              (length *targets*) (length all-results)
              (count :killed all-results :key #'cl-weave:mutation-result-status)
              survived errored)
      (uiop:quit (if (and (zerop empty-targets) (zerop survived) (zerop errored))
                     0
                     1)))))

(main)
