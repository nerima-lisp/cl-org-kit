;;;; t/emitter-test.lisp
(in-package #:cl-org-kit/test)

(declaim (ftype function emitter-test-node-projection
                          emitter-test-value-projection
                          emitter-test-table-el-source))

(defun emitter-test-normalize-string (string)
  (with-output-to-string (stream)
    (loop for position from 0 below (length string)
          for character = (char string position)
          do (if (and (char= character #\Return)
                      (< (1+ position) (length string))
                      (char= (char string (1+ position)) #\Newline))
                 (progn
                   (write-char #\Newline stream)
                   (incf position))
                 (write-char character stream)))))

(defun emitter-test-value-projection (value)
  (typecase value
    (node (emitter-test-node-projection value))
    (string (emitter-test-normalize-string value))
    (cons (mapcar #'emitter-test-value-projection value))
    (vector (map 'list #'emitter-test-value-projection value))
    (t value)))

(defun emitter-test-reader-property-p (key)
  (and (keywordp key)
       (let ((name (symbol-name key)))
         (and (>= (length name) 7)
              (string= "READER-" name :end2 7)))))

(defun emitter-test-table-el-source (node)
  (let* ((source (emitter-test-normalize-string (or (node-text node) "")))
         (begin (node-begin node))
         (affiliated (node-post-affiliated node))
         (prefix-length
           (if (and (integerp begin) (integerp affiliated))
               (- affiliated begin)
               0)))
    (setf source (subseq source prefix-length))
    (let ((end (length source)))
      (loop repeat (node-post-blank node)
            do (when (and (plusp end) (char= (char source (1- end)) #\Newline))
                 (decf end))
               (setf end
                     (1+ (or (position #\Newline source :end end :from-end t) -1))))
      (subseq source 0 end))))

(defun emitter-test-node-projection (node)
  (list (node-type node)
        ;; Headline raw-value duplicates source spelling; title objects carry its structure.
        (loop for (key value) on (node-properties node) by #'cddr
              unless (or (emitter-test-reader-property-p key)
                         (and (eq (node-type node) :headline)
                              (eq key :raw-value)))
                append (list key (emitter-test-value-projection value)))
        (node-post-blank node)
        (when (eq (node-type node) :plain-text)
          (emitter-test-normalize-string (or (node-text node)
                                             (node-property node :value)
                                             "")))
        (when (and (eq (node-type node) :table)
                   (eq (node-property node :type) :table.el))
          (emitter-test-table-el-source node))
        (mapcar #'emitter-test-node-projection (node-children node))))

(defun emitter-test-corpus-files (directory)
  (sort
   (append
    (uiop:directory-files directory)
    (mapcan #'emitter-test-corpus-files (uiop:subdirectories directory)))
   #'string< :key #'namestring))

(defun emitter-test-tree-types (root)
  (let ((types '())
        (seen (make-hash-table :test #'eq)))
    (labels ((visit-value (value)
               (typecase value
                 (node (visit-node value))
                 (cons (mapc #'visit-value value))))
             (visit-node (node)
               (unless (gethash node seen)
                 (setf (gethash node seen) t)
                 (pushnew (node-type node) types)
                 (dolist (child (node-children node))
                   (visit-node child))
                 (loop for tail on (node-properties node) by #'cddr
                       do (visit-value (second tail))))))
      (visit-node root))
    types))

(defun emitter-test-round-trip-file (pathname)
  (let* ((source (parse-org pathname))
         (root (document-root source))
         (expected (emitter-test-node-projection root))
         (output (emit-org root)))
    (unless (stringp output)
      (error "emit-org returned ~S for fixture ~A" output pathname))
    (let* ((parsed (parse-org output))
           (actual (emitter-test-node-projection (document-root parsed))))
      (unless (equal expected actual)
        (error "emit round trip differs for fixture ~A~%Expected: ~S~%Actual: ~S~%Output:~%~A"
               pathname expected actual output))
      (values output (emitter-test-tree-types root)))))

(defun emitter-test-generated-projection (node)
  (list (node-type node)
        (case (node-type node)
          (:headline
           (list :level (node-property node :level)
                 :title (or (and (node-property node :title)
                                 (emitter-test-value-projection
                                  (node-property node :title)))
                            (list (node-property node :raw-value)))))
          (:example-block (node-property node :value)))
        (mapcar #'emitter-test-generated-projection (node-children node))))

(describe "P6 canonical Org emission"
  (it "emits programmatically assembled element trees as strings"
    (let* ((tree
             (make-node
              :org-data
              :children
              (list
               (make-node
                :headline
                :properties (list :level 1 :raw-value "Generated")
                :children
                (list
                 (make-node
                  :section
                  :children
                  (list
                   (make-node
                    :example-block
                    :properties
                    (list :switches nil
                          :value
                          (format nil
                                  "* literal heading~%#+TITLE: literal keyword~%"))))))))))
           (output (emit-org tree))
           (parsed (parse-org output)))
      (with-soft-assertions
        (expect (stringp output) :to-be t)
        (expect output :to-equal
                (format nil "* Generated~%#+BEGIN_EXAMPLE~%* literal heading~%#+TITLE: literal keyword~%#+END_EXAMPLE~%"))
        (expect (emitter-test-generated-projection tree)
                :to-equal
                (emitter-test-generated-projection (document-root parsed))))))

  (it "round trips every recursively discovered corpus fixture and node type"
    (let* ((directory
             (asdf:system-relative-pathname
              "cl-org-kit/test" "t/fixtures/corpus/"))
           (files (emitter-test-corpus-files directory))
           (types '())
           (fixture-count (length files)))
      (format t "~&P6 corpus fixture count: ~D~%" fixture-count)
      (with-soft-assertions
        (expect (> fixture-count 0) :to-be t))
      (dolist (file files)
        (multiple-value-bind (_output file-types)
            (emitter-test-round-trip-file file)
          (declare (ignore _output))
          (setf types (union types file-types))))
      (with-soft-assertions
        (expect (equal (sort (copy-list types) #'string<
                            :key #'symbol-name)
                       (sort (copy-list (node-types)) #'string<
                             :key #'symbol-name))
                :to-be t))))

  (it "enforces the output character limit at and above the boundary"
    (let* ((tree (make-node :keyword
                            :properties (list :key "TITLE"
                                              :value "Sample"
                                              :options nil)))
           (output (emit-org tree))
           (length (length output)))
      (with-soft-assertions
        (expect output :to-equal (format nil "#+TITLE: Sample~%"))
        (expect (length (emit-org tree
                                  :limits (make-limits
                                           :max-output-length length)))
                :to-be length)
        (signals org-resource-limit-error
          (emit-org tree
                    :limits (make-limits
                             :max-output-length (1- length)))))))

  (it "rejects element trees without a faithful Org representation"
    (let ((empty-section (make-node :section))
          (orphan-property
            (make-node :org-data
                       :children
                       (list
                        (make-node :property-drawer
                                   :children
                                   (list (make-node :node-property
                                                    :properties
                                                    (list :key "ID"
                                                          :value "x")))))))
          (closing-marker
            (make-node :example-block
                       :properties
                       (list :switches nil
                             :value (format nil "payload~%#+END_EXAMPLE~%")))))
      (with-soft-assertions
        (signals org-kit-error (emit-org empty-section))
        (signals org-kit-error (emit-org orphan-property))
        (signals org-kit-error (emit-org closing-marker)))))
)
