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
                         ;; The table.el raw body is projected separately from source text below.
                         (and (eq (node-type node) :table)
                              (eq (node-property node :type) :table.el)
                              (eq key :value))
                         ;; Cell :value preserves source spelling; children drive canonical emission.
                         (and (eq (node-type node) :table-cell)
                              (eq key :value))
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

(defun emitter-test-list-item (bullet checkbox text &optional counter tag)
  (make-node :item
             :properties (list :bullet bullet :checkbox checkbox
                               :counter counter :tag tag)
             :children
             (list (make-node :paragraph
                              :children
                              (list (make-node :plain-text
                                               :properties (list :value text)))))))

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

(defun emitter-test-error-message (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (org-kit-error (condition) (org-kit-error-message condition))))

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

  (it "round trips pipe table formulas in their source order"
    (dolist (source (list (format nil "|a|~%#+TBLFM: $1=$1~%")
                          (format nil "|a|~%#+TBLFM: $1=$1+1~%#+TBLFM: $1=$1-1~%")))
      (let* ((root (document-root (parse-org source)))
             (expected (emitter-test-node-projection root))
             (output (emit-org root))
             (actual
               (emitter-test-node-projection
                (document-root (parse-org output)))))
        (with-soft-assertions
          (expect output :to-equal source)
          (expect actual :to-equal expected)))))

  (it "emits table-cell children when their raw value contains timestamp weekday text"
    (let* ((source (format nil "| timestamp <2026-10-12 Mon> |~%"))
           (document (parse-org source))
           (root (document-root document))
           (section (first (node-children root)))
           (table (first (node-children section)))
           (row (first (node-children table)))
           (cell (first (node-children row)))
           (output (emit-org root)))
      (expect (node-property cell :value)
              :to-equal "timestamp <2026-10-12 Mon>")
      (expect output :to-equal (format nil "|timestamp <2026-10-12>|~%"))))

  (it "round trips table.el formulas from the opaque source"
    (let* ((source (format nil "+---+~%| x |~%+---+~%#+TBLFM: $1=$1~%"))
           (root (document-root (parse-org source)))
           (expected (emitter-test-node-projection root))
           (output (emit-org root))
           (actual
             (emitter-test-node-projection
              (document-root (parse-org output)))))
      (with-soft-assertions
        (expect output :to-equal source)
        (expect actual :to-equal expected))))

  (it "rejects malformed pipe table formula properties"
    (let* ((root (document-root (parse-org (format nil "|a|~%"))))
           (table (first (node-children (first (node-children root)))))
           (cycle (list "$1=$1")))
      (setf (cdr cycle) cycle)
      (dolist (formulas (list "not a list" (list 1)
                              (cons "$1=$1" "dotted") cycle))
        (setf (node-property table :tblfm) formulas)
        (signals org-kit-error (emit-org root)))))

  (it "rejects raw-value-only headline titles that parse as inline objects"
    (let ((headline
            (make-node :headline
                       :properties (list :level 1 :raw-value "*x*"))))
      (signals org-kit-error (emit-org headline))))

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

  (it "round trips a special block without emitting nil reader metadata"
    (let* ((source (format nil "#+BEGIN_note~%A note.~%#+END_note~%"))
           (root (document-root (parse-org source)))
           (expected (emitter-test-node-projection root))
           (output (emit-org root))
           (actual
             (emitter-test-node-projection
              (document-root (parse-org output)))))
      (with-soft-assertions
        (expect output :to-equal source)
        (expect actual :to-equal expected))))

  (it "still rejects malformed non-nil affiliated keyword values"
    (let ((tree
            (make-node :org-data
                       :children
                       (list
                        (make-node :special-block
                                   :properties
                                   (list :type "note"
                                         :parameters nil
                                         :name #()))))))
      (signals org-kit-error (emit-org tree))))

  (it "round trips a closed clock whose reader retained no timestamp object"
    (let* ((source (format nil "CLOCK: => 12:30~%"))
           (root (document-root (parse-org source)))
           (expected (emitter-test-node-projection root))
           (output (emit-org root))
           (actual
             (emitter-test-node-projection
              (document-root (parse-org output)))))
      (with-soft-assertions
        (expect output :to-equal (format nil "CLOCK: => 0:00~%"))
        (expect actual :to-equal expected))))

  (it "round trips a running clock without a timestamp object"
    (let* ((source (format nil "CLOCK:~%"))
           (root (document-root (parse-org source)))
           (expected (emitter-test-node-projection root))
           (output (emit-org root))
           (actual
             (emitter-test-node-projection
              (document-root (parse-org output)))))
      (with-soft-assertions
        (expect output :to-equal source)
        (expect actual :to-equal expected))))

  (it "round trips mixed list bullets and a plain-text descriptive tag"
    (let* ((source
             (format nil "- unordered~%3. ordered~%4. also ordered~%+ Term :: descriptive~%"))
           (root (document-root (parse-org source)))
           (tagged-item
             (car
              (last
               (node-children
                (first (node-children (first (node-children root))))))))
           (expected (emitter-test-node-projection root))
           (output (emit-org root))
           (actual
             (emitter-test-node-projection
              (document-root (parse-org output)))))
      (with-soft-assertions
        (expect (node-property tagged-item :tag) :to-equal (list "Term"))
        (expect output :to-equal source)
        (expect actual :to-equal expected))))

  (it "preserves descriptive tag objects and rejects markup-boundary collisions"
    (labels ((tag-object ()
               (make-node :bold
                          :children
                          (list (make-node :plain-text
                                           :properties (list :value "middle")))))
             (tag-projection (value)
               (typecase value
                 (node
                  (if (eq (node-type value) :plain-text)
                      (list :plain-text
                            (or (node-text value) (node-property value :value)))
                      (list (node-type value)
                            (mapcar #'tag-projection (node-children value)))))
                 (cons (mapcar #'tag-projection value))
                 (t value)))
             (item-tag (root)
               (node-property
                (first
                 (node-children
                  (first
                   (node-children
                    (first (node-children root))))))
                :tag)))
      (dolist (tag (list (list (tag-object))
                         (list "left" (tag-object) "right")))
        (let* ((item (make-node :item
                                :properties (list :bullet "+ " :tag tag)
                                :children
                                (list (make-node :paragraph
                                                 :children
                                                 (list (make-node
                                                        :plain-text
                                                        :properties
                                                        (list :value "body")))))))
               (tree (make-node :org-data
                                :children
                                (list (make-node :section
                                                 :children
                                                 (list (make-node
                                                        :plain-list
                                                        :properties
                                                        (list :type :descriptive)
                                                        :children (list item))))))))
          (if (stringp (first tag))
              (signals org-kit-error (emit-org tree))
              (let* ((output (emit-org tree))
                     (parsed-tag (item-tag (document-root (parse-org output)))))
                (expect output :to-equal (format nil "+ *middle* :: body~%"))
                (expect (tag-projection parsed-tag)
                        :to-equal (tag-projection tag))))))))

  (it "rejects a first list bullet that disagrees with its list type"
    (let ((tree
            (make-node :org-data
                       :children
                       (list
                        (make-node :plain-list
                                   :properties (list :type :unordered)
                                   :children
                                   (list
                                    (make-node :item
                                               :properties
                                               (list :bullet "3. "))))))))
      (signals org-kit-error (emit-org tree))))

  (it "emits a detached empty table.el node as a minimal table"
    (let* ((table (make-node :table
                             :properties (list :type :table.el :tblfm nil)))
           (output (emit-org table))
           (parsed-table
             (first
              (node-children
               (first
                (node-children (document-root (parse-org output))))))))
      (with-soft-assertions
        (expect output :to-equal (format nil "+-+~%"))
        (expect (list (node-type parsed-table)
                      (node-property parsed-table :type)
                      (node-property parsed-table :tblfm)
                      (node-children parsed-table))
                :to-equal
                (list (node-type table)
                      (node-property table :type)
                      (node-property table :tblfm)
                      (node-children table))))))
    (let* ((source (format nil "+---+~%| x |~%+---+~%"))
           (document (parse-org source))
           (table (make-node :table
                             :begin 0
                             :end (length source)
                             :properties (list :type :table.el :tblfm nil))))
      (setf (document-root document) table)
      (let* ((output (emit-org table))
             (parsed-root (document-root (parse-org output)))
             (parsed-table (first (node-children (first (node-children parsed-root))))))
        (expect output :to-equal source)
        (expect (emitter-test-node-projection parsed-table)
                :to-equal (emitter-test-node-projection table))))
    (let* ((source (format nil "~%"))
           (document (parse-org source))
           (table (make-node :table
                             :begin 0
                             :end (length source)
                             :post-blank 1
                             :properties (list :type :table.el :tblfm nil))))
      (setf (document-root document) table)
      (signals org-kit-error (emit-org table)))
    (let* ((source (format nil "+---+~%| x |~%+---+~%~%"))
           (document (parse-org source))
           (root (document-root document))
           (expected (emitter-test-node-projection root))
           (output (emit-org root))
           (actual
             (emitter-test-node-projection
              (document-root (parse-org output)))))
      (with-soft-assertions
        (expect output :to-equal source)
        (expect actual :to-equal expected)))
    (dolist (mutation '(:post-blank :post-affiliated))
      (let* ((document (parse-org (format nil "+---+~%| x |~%+---+~%")))
             (root (document-root document))
             (section (first (node-children root)))
             (table (first (node-children section))))
        (ecase mutation
          (:post-blank (setf (org-kit::%node-post-blank table) 1))
          (:post-affiliated
           (setf (org-kit::%node-post-affiliated table)
                 (1+ (length (node-text table))))))
        (signals org-kit-error (emit-org root))))
    (let* ((document (parse-org (format nil "+---+~%| x |~%+---+")))
           (root (document-root document))
           (table (first (node-children (first (node-children root))))))
      (setf (org-kit::%node-post-blank table) 1)
      (signals org-kit-error (emit-org root)))

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

  (it "rejects unrepresentable element trees and raw block closers"
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
            (make-node :comment-block
                       :properties
                       (list :value (format nil "payload~%#+END_COMMENT~%")))))
      (with-soft-assertions
        (signals org-kit-error (emit-org empty-section))
        (signals org-kit-error (emit-org orphan-property))
        (expect (search "payload contains its closing marker #+END_COMMENT"
                        (emitter-test-error-message
                         (lambda () (emit-org closing-marker))))
                :to-be-truthy))))

  (it "emits TODO states from active TYP_TODO declarations in either order"
    (dolist (source
             (list (format nil
                           "#+TODO: SHARED | OLD-DONE~%#+TYP_TODO: OLD-DONE | SHARED~%* SHARED State~%")
                   (format nil
                           "#+TYP_TODO: OLD-DONE | SHARED~%#+TODO: SHARED | OLD-DONE~%* SHARED State~%")))
      (let* ((document (parse-org source))
             (root (document-root document))
             (headline (second (node-children root)))
             (output (emit-org root))
             (emitted-headline
               (second
                (node-children
                 (document-root (parse-org output))))))
        (with-soft-assertions
          (expect (node-property headline :todo-keyword) :to-equal "SHARED")
          (expect (node-property headline :todo-type) :to-equal :done)
          (expect output :to-equal source)
          (expect (node-property emitted-headline :todo-keyword)
                  :to-equal "SHARED")
          (expect (node-property emitted-headline :todo-type)
                  :to-equal :done)))))

  (it "keeps unique TYP_TODO keywords when merging emitted TODO settings"
    (let* ((source
             (format nil
                     "#+TODO: TODO | DONE~%#+TYP_TODO: IDEA | REJECTED~%* IDEA Item~%"))
           (document (parse-org source))
           (output (emit-org (document-root document)))
           (emitted (parse-org output)))
      (with-soft-assertions
        (expect output :to-equal source)
        (expect (getf (document-settings emitted) :todo-keywords)
                :to-equal '(("IDEA" . :todo) ("REJECTED" . :done))))))

  (it "keeps the first repeated ordinary TODO keyword"
    (expect (org-kit::emitter-merge-todo-keywords
             '(("SHARED" . :todo) ("SHARED" . :done) ("WAIT" . :todo))
             '(("TYPED" . :done)))
            :to-equal '(("SHARED" . :todo) ("WAIT" . :todo)
                        ("TYPED" . :done))))

  (it "preserves structured headline titles and document heading settings"
    (let* ((tree
             (make-node
              :org-data
              :children
              (list
               (make-node :section
                          :children
                          (list (make-node :keyword
                                           :properties (list :key "TODO"
                                                             :value "WAIT | DONE"
                                                             :options nil))))
               (make-node
                :headline
                :properties
                (list :level 1
                      :todo-keyword "WAIT"
                      :todo-type :todo
                      :priority 65
                      :title (list "Before "
                                   (make-node :bold
                                              :children
                                              (list (make-node
                                                     :plain-text
                                                     :properties (list :value "bold"))))
                                   " after"))))))
           (output (emit-org tree)))
      (with-soft-assertions
        (expect output :to-equal
                (format nil "#+TODO: WAIT | DONE~%* WAIT [#A] Before *bold* after~%"))
        (let* ((parsed (parse-org output))
               (reparsed (parse-org (emit-org (document-root parsed))))
               (headline (second (node-children (document-root parsed)))))
          (expect (node-type headline) :to-equal :headline)
          (expect (some (lambda (segment)
                          (and (node-p segment)
                               (eq (node-type segment) :bold)))
                        (node-property headline :title))
                  :to-be t)
          (expect (emitter-test-node-projection (document-root parsed))
                  :to-equal
                  (emitter-test-node-projection (document-root reparsed))))))
    (expect (emit-org (make-node :headline
                                 :properties (list :level 1
                                                   :raw-value "Archived"
                                                   :tags (list "ARCHIVE" "under_score")
                                                   :archivedp t)))
            :to-equal (format nil "* Archived :ARCHIVE:under_score:~%"))
    (let* ((output (emit-org (make-node :headline
                                        :properties (list :level 1
                                                          :raw-value "Numeric priority"
                                                          :priority 64))))
           (headline (first (node-children
                             (document-root (parse-org output))))))
      (expect output :to-equal (format nil "* [#64] Numeric priority~%"))
      (expect (node-property headline :priority) :to-be 64))
    (let ((tree
            (make-node
             :org-data
             :children
             (list
              (make-node :keyword
                         :properties (list :key "STARTUP"
                                           :value "odd"
                                           :options nil))
              (make-node :headline
                         :properties (list :level 2 :raw-value "Odd level"))))))
      (expect (emit-org tree)
              :to-equal (format nil "#+STARTUP: odd~%*** Odd level~%")))
    (let* ((tree
             (make-node
              :org-data
              :children
              (list (make-node :section
                               :children
                               (list (make-node :keyword
                                                :properties (list :key "STARTUP"
                                                                  :value "showall"
                                                                  :options nil))))
                    (make-node :headline
                               :properties (list :level 2
                                                 :raw-value "Even level")))))
           (output (emit-org tree))
           (parsed-root (document-root (parse-org output))))
      (expect output
              :to-equal (format nil "#+STARTUP: showall~%** Even level~%"))
      (expect (node-property
               (first (node-children (first (node-children parsed-root))))
               :value)
              :to-equal "showall")
      (expect (node-property (second (node-children parsed-root)) :level)
              :to-be 2)))

  (it "honors the final odd-level startup option across tokens and lines"
    (dolist (source
             (list (format nil "#+STARTUP: odd oddeven~%** Heading~%")
                   (format nil "#+STARTUP: odd~%#+STARTUP: oddeven~%** Heading~%")))
      (let* ((document (parse-org source))
             (root (document-root document))
             (output (emit-org root))
             (round-trip (document-root (parse-org output))))
        (with-soft-assertions
          (expect output :to-equal source)
          (expect (emitter-test-node-projection root)
                  :to-equal (emitter-test-node-projection round-trip))))))

  (it "preserves title text that follows emitted headline metadata"
    (dolist (headline
             (list
              (make-node :headline
                         :properties (list :level 1
                                           :todo-keyword "TODO"
                                           :todo-type :todo
                                           :title (list "TODO task")))
              (make-node :headline
                         :properties (list :level 1
                                           :priority 65
                                           :title (list "[#A] task")))
              (make-node :headline
                         :properties (list :level 1
                                           :commentedp t
                                           :title (list "COMMENT task")))
              (make-node :headline
                         :properties (list :level 1
                                           :tags (list "outer")
                                           :title (list "task :inner:")))))
      (let* ((output (emit-org headline))
             (parsed (first (node-children (document-root (parse-org output))))))
        (expect (emitter-test-generated-projection parsed)
                :to-equal (emitter-test-generated-projection headline))
        (dolist (key '(:todo-keyword :todo-type :priority :commentedp :tags))
          (expect (node-property parsed key)
                  :to-equal (node-property headline key))))))

  (it "rejects headline metadata that would be reparsed differently"
    (dolist (headline
             (list
              (make-node :headline :properties (list :level 0 :raw-value "Bad"))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :commentedp :yes))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :archivedp :yes))
              (make-node :inlinetask :properties (list :level 14 :raw-value "Bad"))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :todo-keyword "WAIT"
                                                    :todo-type :todo))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :todo-keyword 7
                                                    :todo-type :todo))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :todo-type :todo))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :priority 0))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :priority 91))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :tags "tag"))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :tags (list 7)))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :tags (list "bad tag")))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :archivedp t))
              (make-node :headline :properties (list :level 1 :raw-value "Bad"
                                                    :tags (list "ARCHIVE")))
              (make-node :headline :properties (list :level 1
                                                    :raw-value (format nil "line one~%line two")))
              (make-node :headline :properties (list :level 1
                                                    :raw-value (format nil "line one~Cline two" #\Return)))
              (make-node :headline :properties (list :level 1 :title "not a list"))
              (make-node :headline :properties
                         (list :level 1
                               :title (list (make-node :section))))
              (make-node :headline :properties
                         (list :level 1 :title (list 7)))
              (make-node :headline :properties (list :level 1
                                                    :title (list " leading")))
              (make-node :headline :properties (list :level 1
                                                    :title (list "*literal*")))
              (make-node :headline :properties (list :level 1
                                                    :title (list "TODO task")))
              (make-node :headline :properties (list :level 1
                                                    :title (list "COMMENT task")))
              (make-node :headline :properties (list :level 1
                                                    :title (list "[#A] task")))
              (make-node :headline :properties (list :level 1
                                                    :title (list "task :tag:")))))
      (signals org-kit-error (emit-org headline)))
    (let ((tree
            (make-node :org-data
                       :children
                       (list (make-node :keyword
                                        :properties (list :key "STARTUP"
                                                          :value "odd"
                                                          :options nil))
                             (make-node :headline
                                        :properties (list :level 1
                                                          :raw-value "Odd level"))))))
      (expect (emit-org tree)
              :to-equal (format nil "#+STARTUP: odd~%* Odd level~%")))
    (let* ((headline (make-node :headline
                                :properties (list :level 1
                                                  :raw-value ""
                                                  :title nil)))
           (output (emit-org headline))
           (parsed-headline
             (first (node-children (document-root (parse-org output))))))
      (expect output :to-equal (format nil "* ~%"))
      (expect (emitter-test-generated-projection parsed-headline)
              :to-equal (emitter-test-generated-projection headline))))

  (it "keeps affiliated metadata and raw block line endings exact"
    (let* ((block
             (make-node
              :example-block
              :properties
              (list "ignored" "value"
                    :name "sample"
                    :caption "Caption"
                    :header "Header"
                    :plot "Plot"
                    :results "Results"
                    :attr_html "<section>"
                    :switches nil
                    :value (format nil "one~C~Ctwo~C~C" #\Return #\Newline
                                   #\Return #\Newline))))
           (output (emit-org (make-node :org-data :children (list block)))))
      (expect output :to-equal
              (format nil "#+ATTR_HTML: <section>~%#+RESULTS: Results~%#+PLOT: Plot~%#+HEADER: Header~%#+CAPTION: Caption~%#+NAME: sample~%#+BEGIN_EXAMPLE~%one~%two~%#+END_EXAMPLE~%")))
    (dolist (node
             (list
              (make-node :comment :properties
                         (list :value (format nil "first line~%second")))
              (make-node :comment :properties (list :value (format nil "first~Csecond" #\Return)))
              (make-node :keyword :properties (list :key "TITLE" :options nil))
              (make-node :keyword :properties (list :key "TITLE:EXTRA"
                                                    :value "x" :options nil))
              (make-node :keyword :properties (list :key "title"
                                                    :value "x" :options nil))
              (make-node :keyword :properties (list :key "TITLE" :value "x"
                                                    :options "[x]"))
              (make-node :keyword :properties (list :key 7 :value "odd"))
              (make-node :diary-sexp :properties (list :value "not a diary"))
              (make-node :diary-sexp :properties (list :value "%%"))
              (make-node :example-block :properties (list :switches "-n"
                                                          :value (format nil "body~%")))
              (make-node :export-block :properties (list :type "html"
                                                        :value (format nil "body~%")))
              (make-node :src-block :properties (list :parameters "-n"))
              (make-node :src-block :properties (list :language "c lang"
                                                      :value (format nil "body~%")))
              (make-node :src-block :properties (list :language "lisp"
                                                      :switches 4
                                                      :value (format nil "body~%")))
              (make-node :src-block :properties (list :language "lisp"
                                                      :switches ""
                                                      :value (format nil "body~%")))
              (make-node :src-block :properties (list :language "lisp"
                                                      :switches " padded "
                                                      :value (format nil "body~%")))
              (make-node :src-block :properties (list :language ""
                                                      :value (format nil "body~%")))
              (make-node :src-block :properties (list :language ""
                                                      :switches "-n"
                                                      :value (format nil "body~%")))
              (make-node :paragraph)
              (make-node :verse-block :children (list (make-node :paragraph)))
              (make-node :example-block :properties (list :switches nil :value "no final newline"))
              (make-node :example-block :properties (list :switches nil
                                                          :value (format nil "bare~Creturn~%" #\Return)))
              (make-node :example-block :properties
                         (list :switches nil
                               :value (concatenate 'string "bare-ending" (string #\Return))))
              (make-node :keyword :properties (list :key "TITLE"
                                                    :value " padded "
                                                    :options nil))))
      (signals org-kit-error (emit-org node)))
    (expect (emit-org (make-node :comment :properties (list :value " padded ")))
            :to-equal (format nil "#  padded ~%"))
    (let ((block (make-node :example-block
                            :properties (list "ATTR_HTML" "ignored"
                                              :switches nil
                                              :value (format nil "body~%")))))
      (expect (emit-org block)
              :to-equal (format nil "#+BEGIN_EXAMPLE~%body~%#+END_EXAMPLE~%")))
    (let* ((block (make-node :export-block
                             :properties (list :type ""
                                               :value (format nil "body~%"))))
           (output (emit-org block))
           (parsed (first (node-children
                           (first (node-children
                                   (document-root (parse-org output))))))))
      (expect output
              :to-equal (format nil "#+BEGIN_EXPORT~%body~%#+END_EXPORT~%"))
      (expect (emitter-test-node-projection parsed)
              :to-equal (emitter-test-node-projection block)))
    (expect (emit-org (make-node :example-block
                                 :properties (list :switches nil
                                                   :value (format nil "payload~C~C" #\Return #\Newline))))
            :to-equal
            (format nil "#+BEGIN_EXAMPLE~%payload~%#+END_EXAMPLE~%")))

  (it "round trips parser-escaped example-block lines through the public API"
    (let* ((source
             (format nil
                     (concatenate 'string
                                  "#+BEGIN_EXAMPLE~%"
                                  ",#+END_EXAMPLE~%"
                                  ",,* literal heading~%"
                                  ",,#+TITLE: literal keyword~%"
                                  "#+END_EXAMPLE~%")))
           (root (document-root (parse-org source)))
           (expected (emitter-test-node-projection root))
           (output (emit-org root))
           (actual
             (emitter-test-node-projection
              (document-root (parse-org output)))))
      (with-soft-assertions
        (expect output :to-equal source)
        (expect actual :to-equal expected))))

  (it "round trips LaTeX environments and rejects incomplete boundaries"
    (let* ((source (format nil "\\begin{align}~%a &= b~%\\end{align}~%"))
           (node (make-node :latex-environment :properties (list :value source)))
           (output (emit-org node))
           (round-trip (parse-org output)))
      (with-soft-assertions
        (expect output :to-equal source)
        (expect (not (null (member :latex-environment
                                   (emitter-test-tree-types
                                    (document-root round-trip)))))
                :to-be t)))
    (dolist (value (list "\\begin{align}"
                         (format nil "\\begin{align}~%")
                         (format nil "\\begin{align}~%\\end{align}")
                         (format nil "\\begin{align}~%body~%")
                         (format nil "\\begin{align}~%\\end{align}~%body~%")
                         (format nil "not a begin line~%\\end{align}~%")))
      (signals org-kit-error
        (emit-org (make-node :latex-environment :properties (list :value value)))))
    )

  (it "rejects block delimiters and names that cannot be represented faithfully"
    (dolist (node
             (list
              (make-node :special-block :properties (list :type "Note"))
              (make-node :special-block :properties (list :type "quote"))
              (make-node :special-block :properties (list :type ""))
              (make-node :special-block :properties (list :type "bad name"))
              (make-node :special-block :properties (list :type "note" :parameters 4))
              (make-node :special-block :properties (list :type "note" :parameters ""))
              (make-node :special-block :properties (list :type "note" :parameters " padded "))
              (make-node :drawer :properties (list :drawer-name "lower"))
              (make-node :drawer :properties (list :drawer-name ""))
              (make-node :drawer :properties (list :drawer-name "END"))
              (make-node :drawer :properties (list :drawer-name "BAD:NAME"))
              (make-node :dynamic-block :properties (list :block-name "Upper"))
              (make-node :dynamic-block :properties (list :block-name "bad name"))
              (make-node :dynamic-block :properties (list :block-name "name"
                                                         :arguments 4))
              (make-node :dynamic-block :properties (list :block-name "name"
                                                         :arguments ""))
              (make-node :dynamic-block :properties (list :block-name "name"
                                                         :arguments " padded "))
              (make-node :footnote-definition :properties (list :label ""))
              (make-node :footnote-definition :properties (list :label "has]close"))))
      (signals org-kit-error (emit-org node)))
    (with-soft-assertions
      (expect (emit-org (make-node :special-block
                                   :properties (list :type "note"
                                                     :parameters "arg")))
              :to-equal (format nil "#+BEGIN_note arg~%#+END_note~%"))
      (expect (emit-org (make-node :drawer
                                   :properties (list :drawer-name "LOGBOOK")))
              :to-equal (format nil ":LOGBOOK:~%:END:~%"))
      (expect (emit-org (make-node :dynamic-block
                                   :properties (list :block-name "name"
                                                     :arguments "arg")))
              :to-equal (format nil "#+BEGIN: name arg~%#+END:~%"))
      (expect (emit-org (make-node :footnote-definition
                                   :properties (list :label "note")))
              :to-equal (format nil "[fn:note] ~%"))))

  (it "rejects drawers that would be parsed as property drawers"
    (dolist (tree
             (list
              (make-node :headline
                         :properties (list :level 1 :raw-value "Heading")
                         :children
                         (list (make-node :section
                                          :children
                                          (list (make-node :drawer
                                                           :properties (list :drawer-name "PROPERTIES"))))))
              (let* ((root (document-root
                            (parse-org (format nil "* Heading~%SCHEDULED: <2026-10-13 Tue>~%"))))
                     (headline (first (node-children root)))
                     (section (first (node-children headline))))
                (setf (node-children section)
                      (append (node-children section)
                              (list (make-node :drawer
                                               :properties (list :drawer-name "PROPERTIES")))))
                root)))
      (signals org-kit-error (emit-org tree))))

  (it "preserves PROPERTIES drawers outside headline property positions"
    (let* ((source (format nil "* Heading~%body~%"))
           (root (document-root (parse-org source)))
           (headline (first (node-children root)))
           (section (first (node-children headline)))
           (drawer (make-node :drawer :properties (list :drawer-name "PROPERTIES"))))
      (setf (node-children section)
            (append (node-children section) (list drawer)))
      (let* ((output (emit-org root))
             (parsed-root (document-root (parse-org output))))
        (expect output
                :to-equal (format nil "* Heading~%body~%:PROPERTIES:~%:END:~%"))
        (expect (emitter-test-node-projection parsed-root)
                :to-equal (emitter-test-node-projection root))))
    (let* ((tree (make-node :org-data
                            :children
                            (list (make-node :section
                                             :children
                                             (list (make-node
                                                    :drawer
                                                    :properties
                                                    (list :drawer-name "PROPERTIES")))))))
           (output (emit-org tree))
           (parsed-root (document-root (parse-org output))))
      (expect output :to-equal (format nil ":PROPERTIES:~%:END:~%"))
      (expect (emitter-test-node-projection parsed-root)
              :to-equal (emitter-test-node-projection tree)))
    (dolist (tree
             (list (make-node :org-data
                              :children
                              (list (make-node :drawer
                                               :properties
                                               (list :drawer-name "PROPERTIES"))))
                   (make-node :section
                              :children
                              (list (make-node :drawer
                                               :properties
                                               (list :drawer-name "PROPERTIES"))))))
      (let* ((output (emit-org tree))
             (parsed-types (emitter-test-tree-types
                            (document-root (parse-org output)))))
        (expect output :to-equal (format nil ":PROPERTIES:~%:END:~%"))
        (expect (not (null (member :drawer parsed-types))) :to-be t)
        (expect (member :property-drawer parsed-types) :to-be nil)))
    (expect (emit-org (make-node :drawer
                                 :properties (list :drawer-name "PROPERTIES")))
            :to-equal (format nil ":PROPERTIES:~%:END:~%")))

  (it "preserves valid property drawers and rejects malformed drawer contents"
    (let* ((tree
             (make-node
              :headline
              :properties (list :level 1 :raw-value "Heading")
              :children
              (list
               (make-node
                :section
                :children
                (list
                 (make-node :property-drawer
                            :children
                            (list (make-node :node-property
                                             :properties (list :key "ID"
                                                               :value "item-1")))))))))
           (output (emit-org tree)))
      (expect output :to-equal
              (format nil "* Heading~%:PROPERTIES:~%:ID: item-1~%:END:~%")))
    (signals org-kit-error
      (emit-org
       (make-node :headline
                  :properties (list :level 1 :raw-value "Heading")
                  :children
                  (list (make-node
                         :section
                         :children
                         (list (make-node :paragraph
                                          :children
                                          (list (make-node
                                                 :plain-text
                                                 :properties (list :value "body"))))
                               (make-node :property-drawer)))))))
    (signals org-kit-error (emit-org (make-node :property-drawer)))
    (expect (emitter-test-error-message
             (lambda ()
               (emit-org (make-node :node-property
                                    :properties (list :key "ID" :value "x")))))
            :to-equal "Cannot emit node-property: node properties must be inside a property drawer")
    (let* ((drawer (make-node :property-drawer
                              :children (list (make-node :comment
                                                         :properties (list :value "not a property")))))
           (section (make-node :section :children (list drawer)))
           (headline (make-node :headline
                                :properties (list :level 1 :raw-value "Heading")
                                :children (list section))))
      (expect (emitter-test-error-message (lambda () (emit-org headline)))
              :to-equal "Cannot emit property-drawer: property drawers can contain only node properties"))
    (dolist (key '("" "lower" "BAD:KEY"))
      (expect (emitter-test-error-message
               (lambda ()
                 (emit-org
                  (make-node :headline
                             :properties (list :level 1 :raw-value "Heading")
                             :children
                             (list (make-node :section
                                              :children
                                              (list (make-node :property-drawer
                                                               :children
                                                               (list (make-node :node-property
                                                                                :properties (list :key key :value "x")))))))))))
              :to-equal "Cannot emit node-property: property name must be uppercase and cannot contain a colon"))
    )

  (it "rejects ambiguous list items and preserves all ordered checkbox states"
      (let* ((tree
               (make-node :plain-list
                          :properties (list :type :ordered)
                          :children
                          (list (emitter-test-list-item "1. " :off "off" 4)
                                (emitter-test-list-item "2) " :on "on")
                                (emitter-test-list-item "3. " :trans "trans"))))
             (output (emit-org tree)))
        (expect output :to-equal
                (format nil "1. [ ] [@4] off~%2) [X] on~%3. [-] trans~%")))
      (let* ((tree
               (make-node :plain-list
                          :properties (list :type :descriptive)
                          :children
                          (list (make-node :item
                                           :properties (list :bullet "+ "
                                                             :tag (list "Term "
                                                                        (make-node
                                                                         :bold
                                                                         :children
                                                                         (list (make-node
                                                                                :plain-text
                                                                                :properties (list :value "bold"))))
                                                                        " after"))
                                           :children (list (make-node :paragraph
                                                                      :children
                                                                      (list (make-node :plain-text
                                                                                       :properties (list :value "description")))))))))
             (output (emit-org tree)))
        (expect output :to-equal
                (format nil "+ Term *bold* after :: description~%")))
      (expect (emit-org (make-node :plain-list
                                   :properties (list :type :unordered)
                                   :children (list (make-node :item
                                                              :properties (list :bullet "- ")))))
              :to-equal (format nil "- ~%"))
      (let* ((inner
               (make-node :plain-list
                          :properties (list :type :unordered)
                          :children (list (emitter-test-list-item "- " nil "inner"))))
             (tree
               (make-node :plain-list
                          :properties (list :type :unordered)
                          :children (list (make-node :item
                                                     :properties (list :bullet "- ")
                                                     :children (list inner))))))
        (expect (emit-org tree)
                :to-equal (format nil "- ~%  - inner~%")))
      (let* ((tree
               (make-node
                :plain-list
                :properties (list :type :unordered)
                :children
                (list (make-node :item
                                 :properties (list :bullet "- ")
                                 :children
                                 (list (make-node :paragraph
                                                  :post-blank 1
                                                  :children (list (make-node :plain-text
                                                                             :properties (list :value "first"))))
                                       (make-node :paragraph
                                                  :children (list (make-node
                                                                   :plain-text
                                                                   :properties (list :value " second"))))))))))
        (expect (emit-org tree)
                :to-equal (format nil "- first~%~% second~%")))
    (dolist (tree
             (list
              (make-node :plain-list :properties (list :type :unknown)
                         :children (list (make-node :item :properties (list :bullet "- "))))
              (make-node :plain-list :properties (list :type :unordered)
                         :children (list (make-node :keyword :properties (list :key "TITLE" :value "x"))))
              (make-node :plain-list :properties (list :type :unordered))
              (make-node :plain-list :properties (list :type :descriptive)
                         :children (list (make-node :item :properties (list :bullet "+ "))))
              (make-node :plain-list :properties (list :type :unordered)
                         :children (list (make-node :item :properties (list :bullet "bad"))))
              (make-node :plain-list :properties (list :type :unordered)
                         :children (list (make-node :item :properties (list :bullet nil))))
              (make-node :plain-list :properties (list :type :unordered)
                         :children (list (make-node :item :properties (list :bullet "- " :checkbox :unknown))))
              (make-node :plain-list :properties (list :type :unordered)
                         :children (list (make-node :item :properties (list :bullet "- " :counter 0))))
              (make-node :plain-list :properties (list :type :descriptive)
                         :children (list (make-node :item :properties (list :bullet "+ " :tag "Tag"))))
              (make-node :plain-list :properties (list :type :descriptive)
                         :children (list (make-node :item :properties (list :bullet "+ " :tag (list 7)))))
              (make-node :plain-list :properties (list :type :descriptive)
                         :children (list (make-node :item :properties (list :bullet "+ " :tag (list (make-node :section))))))
              (make-node :plain-list :properties (list :type :descriptive)
                         :children (list (make-node :item :properties (list :bullet "+ " :tag (list "has :: delimiter")))))
              (make-node :plain-list :properties (list :type :descriptive)
                         :children (list (make-node :item :properties (list :bullet "+ " :tag (list (format nil "line~%"))))))
              (make-node :plain-list :properties (list :type :descriptive)
                         :children (list (make-node :item :properties (list :bullet "+ " :tag (list (format nil "line~C" #\Return))))))
              (make-node :plain-list :properties (list :type :descriptive)
                         :children (list (make-node :item :properties (list :bullet "+ " :tag (list " padded ")))))
              (make-node :plain-list :properties (list :type :unordered)
                         :children (list (make-node :item :properties (list :bullet "* "))))
              (make-node :item :properties (list :bullet "- "))
              (make-node :plain-list
                         :properties (list :type :unordered)
                         :children
                         (list (make-node :item :properties (list :bullet "- ")
                                          :children (list (make-node :keyword
                                                                     :properties (list :key "TITLE" :value "x"))))))
              (make-node :plain-list
                         :properties (list :type :unordered)
                         :children
                         (list (make-node :item :properties (list :bullet "- ")
                                          :children (list (make-node :paragraph
                                                                     :properties (list :name "nested")
                                                                     :children (list (make-node :plain-text
                                                                                               :properties (list :value "x"))))))))
              (make-node :plain-list
                         :properties (list :type :unordered)
                         :children
                         (list (make-node :item :properties (list :bullet "- ")
                                          :children (list (make-node :plain-list
                                                                     :properties (list :type :unordered)
                                                                     :children (list (make-node :item :properties (list :bullet "- "))))
                                                        (make-node :plain-list
                                                                   :properties (list :type :unordered)
                                                                   :children (list (make-node :item :properties (list :bullet "- "))))))))))
      (signals org-kit-error (emit-org tree))))

  (it "rejects list paragraph whitespace and ambiguous paragraph boundaries"
    (let* ((paragraph (lambda (text &optional (post-blank 0))
                        (make-node :paragraph
                                   :post-blank post-blank
                                   :children (list (make-node :plain-text
                                                              :properties (list :value text))))))
           (bad-first
             (make-node :plain-list
                        :properties (list :type :unordered)
                        :children (list (make-node :item
                                                   :properties (list :bullet "- ")
                                                   :children (list (funcall paragraph " leading"))))))
           (bad-continuation
             (make-node :plain-list
                        :properties (list :type :unordered)
                        :children (list (make-node :item
                                                   :properties (list :bullet "- ")
                                                   :children (list (funcall paragraph
                                                                            (format nil "first~%second")))))))
           (bad-boundary
             (make-node :plain-list
                        :properties (list :type :unordered)
                        :children (list (make-node :item
                                                   :properties (list :bullet "- ")
                                                   :children (list (funcall paragraph "first")
                                                                   (funcall paragraph "second")))))))
      (signals org-kit-error (emit-org bad-first))
      (signals org-kit-error (emit-org bad-continuation))
      (signals org-kit-error (emit-org bad-boundary))))

  (it "keeps table rows and clock metadata representable"
    (let* ((table
             (make-node :table
                        :properties (list :type :org)
                        :children
                        (list (make-node :table-row :properties (list :type :rule))
                              (make-node :table-row
                                         :properties (list :type :standard)
                                         :children
                                         (list (make-node :table-cell
                                                          :children (list (make-node :plain-text
                                                                                     :properties (list :value "left"))))
                                               (make-node :table-cell
                                                          :children (list (make-node :plain-text
                                                                                     :properties (list :value "right")))))))))
           (output (emit-org table)))
      (expect output :to-equal
              (format nil "|-----|~%|left|right|~%")))
    (dolist (node
             (list
              (make-node :table :properties (list :type :org)
                         :children nil)
              (make-node :table :properties (list :type :table.el :tblfm nil)
                         :children (list (make-node :table-row
                                                    :properties (list :type :standard))))
              (make-node :table :properties (list :type :org)
                         :children (list (make-node :keyword :properties (list :key "TITLE" :value "x"))))
              (make-node :table :properties (list :type :table.el :tblfm "formula"))
              (make-node :table :properties (list :type :unknown))
              (make-node :table :properties (list :type :org)
                         :children (list (make-node :table-row
                                                    :properties (list :type :rule)
                                                    :children (list (make-node :table-cell
                                                                               :properties (list :value "x"))))))
              (make-node :table :properties (list :type :org)
                         :children (list (make-node :table-row
                                                    :properties (list :type :standard)
                                                    :children (list (make-node :keyword
                                                                               :properties (list :key "TITLE" :value "x"))))))
              (make-node :table :properties (list :type :org)
                         :children (list (make-node :table-row :properties (list :type :unknown))))
              (make-node :table-row :properties (list :type :rule))
              (make-node :clock :properties (list :value "not a timestamp" :status :running))
              (make-node :clock :properties (list :value nil :status :unknown))))
      (signals org-kit-error (emit-org node))))

  (it "emits multiple planning entries and rejects detached planning"
    (let* ((source (format nil "* Task~%SCHEDULED: <2026-10-13> DEADLINE: <2026-10-14>~%"))
           (root (document-root (parse-org source)))
           (output (emit-org root)))
      (expect output :to-equal source)
      (expect (emitter-test-node-projection (document-root (parse-org output)))
              :to-equal (emitter-test-node-projection root)))
    (flet ((wrap (planning)
             (make-node :headline
                        :properties (list :level 1 :raw-value "Task")
                        :children (list (make-node :section
                                                   :children (list planning))))))
      (signals org-kit-error
        (emit-org (make-node :planning :properties (list :scheduled "not a timestamp"))))
      (signals org-kit-error (emit-org (wrap (make-node :planning))))
      (signals org-kit-error
        (emit-org (wrap (make-node :planning
                                   :properties (list :scheduled "not a timestamp")))))))

  (it "rejects planning outside the first position in a headline section"
    (signals org-kit-error
      (emit-org (make-node :org-data
                           :children
                           (list (make-node :planning
                                            :properties
                                            (list :scheduled "<2026-10-13>"))))))
    (signals org-kit-error
      (emit-org (make-node :section
                           :children
                           (list (make-node :planning
                                            :properties
                                            (list :scheduled "<2026-10-13>"))))))
    (let ((headline
            (make-node :headline
                       :properties (list :level 1 :raw-value "Task")
                       :children
                       (list (make-node
                              :section
                              :children
                              (list (make-node :paragraph
                                               :children
                                               (list (make-node
                                                      :plain-text
                                                      :properties
                                                      (list :value "body"))))
                                    (make-node :planning
                                               :properties
                                               (list :scheduled "<2026-10-13>"))))))))
      (signals org-kit-error (emit-org headline))))

  (it "rejects malformed Babel calls and preserves valid call syntax"
    (expect (emit-org (make-node :babel-call
                                 :properties (list :call "result" :arguments "x,y")))
            :to-equal (format nil "#+CALL: result(x,y)~%"))
    (dolist (node
             (list
              (make-node :babel-call :properties (list :call "run" :inside-header t))
              (make-node :babel-call :properties (list :call "run" :end-header t))
              (make-node :babel-call :properties (list :call " run"))
              (make-node :babel-call :properties (list :call "run("))
              (make-node :babel-call :properties (list :call "run)"))
              (make-node :babel-call :properties (list :call "run()"))
              (make-node :babel-call :properties (list :call "run" :arguments ""))
              (make-node :babel-call :properties (list :call "run" :arguments " padded "))
              (make-node :babel-call :properties (list :call "run" :arguments "a(b"))
              (make-node :babel-call :properties (list :call "run" :arguments "a)b"))))
      (signals org-kit-error (emit-org node))))

  (it "rejects inline radio targets that would capture plain text"
    (let ((target (make-node :radio-target
                             :properties (list :value "ref")
                             :children (list (make-node
                                              :plain-text
                                              :properties (list :value "ref"))))))
      (expect (emit-org
               (make-node :paragraph
                          :children
                          (list target
                                (make-node :plain-text
                                           :properties (list :value "xref refx")))))
              :to-equal (format nil "<<<ref>>>xref refx~%")))
    (signals org-kit-error
      (emit-org
       (make-node :paragraph
                  :children
                  (list (make-node :radio-target
                                   :properties (list :value "ref")
                                   :children (list (make-node
                                                   :plain-text
                                                   :properties (list :value "ref"))))
                        (make-node :plain-text :properties (list :value " ref ")))))))

  (it "validates radio target values before registering them"
    (signals org-kit-error
      (emit-org (make-node :radio-target :properties (list :value 7)))))

  (it "checks radio target capture inside structured headline titles"
    (let* ((target (make-node :radio-target
                              :properties (list :value "ref")
                              :children (list (make-node
                                               :plain-text
                                               :properties (list :value "ref")))))
           (safe (make-node :headline
                            :properties (list :level 1
                                              :title (list target " "
                                                           (make-node :plain-text
                                                                      :properties (list :value "other"))))))
           (output (emit-org safe)))
      (expect output :to-equal (format nil "* <<<ref>>> other~%")))
    (let* ((target (make-node :radio-target
                              :properties (list :value "ref")
                              :children (list (make-node
                                               :plain-text
                                               :properties (list :value "ref")))))
           (unsafe (make-node :headline
                              :properties (list :level 1
                                                :title (list target " "
                                                             (make-node :plain-text
                                                                        :properties (list :value "ref")))))))
      (signals org-kit-error (emit-org unsafe))))

  (it "rejects cyclic element trees before serialization"
    (let ((tree (make-node :org-data)))
      (setf (node-children tree) (list tree))
      (signals org-kit-error (emit-org tree)))
    (let ((clock (make-node :clock :properties (list :status :running))))
      (setf (node-property clock :value) clock)
      (expect (emitter-test-error-message (lambda () (emit-org clock)))
              :to-equal "Cannot emit clock: the node tree contains a cycle")))
)
