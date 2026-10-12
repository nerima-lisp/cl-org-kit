;;;; run-tests.lisp
;;;;
;;;; Register this checkout, fail on compile warnings, then run the test system.
(require :asdf)

(defun script-directory ()
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *compile-file-truename*
                               (error "Unable to determine script location"))))

(asdf:initialize-source-registry
 `(:source-registry (:tree ,(script-directory)) :inherit-configuration))

(defun compile-warning-check ()
  "Signal an error when compiling the systems or the scripts warns.
Any non-style warning fails, and so does a style warning raised from src/."
  (let* ((root (script-directory))
         (source-directory (merge-pathnames "src/" root))
         (files (list (merge-pathnames "run-tests.lisp" root)))
         (warnings nil))
    (flet ((record-warning (condition)
             (when *compile-file-truename*
               (push (list *compile-file-truename* condition) warnings)
               (muffle-warning condition))))
      (handler-bind ((warning #'record-warning))
        (asdf:operate 'asdf:compile-op "cl-org-kit" :force t)
        (asdf:operate 'asdf:compile-op "cl-org-kit/test" :force t)
        (dolist (file files)
          (compile-file file
                        :output-file
                        (merge-pathnames
                         (make-pathname :name (format nil "~A-check" (pathname-name file))
                                        :type "fasl")
                         (uiop:temporary-directory))))))
    (let ((non-style (remove-if (lambda (entry)
                                  (typep (second entry) 'style-warning))
                                warnings))
          (source-style (remove-if-not
                         (lambda (entry)
                           (and (typep (second entry) 'style-warning)
                                (uiop:string-prefix-p
                                 (namestring source-directory)
                                 (namestring (first entry)))))
                         warnings)))
      (format t "compile warning check: non-style=~D src-style=~D~%"
              (length non-style) (length source-style))
      (dolist (entry (append non-style source-style))
        (format *error-output* "~&compile warning in ~A: ~A~%"
                (namestring (first entry)) (second entry)))
      (when (or non-style source-style)
        (error "Compilation warning check failed: non-style=~D src-style=~D"
               (length non-style) (length source-style))))))

(compile-warning-check)
(asdf:test-system "cl-org-kit")
(uiop:quit 0)
