;;;; src/outline.lisp
(in-package #:org-kit)

(defstruct (outline-entry (:constructor %make-outline-entry)
                          (:conc-name %outline-entry-)
                          (:copier nil)
                          (:predicate %outline-entry-p))
  "One headline or source/example block listed by OUTLINE."
  (kind nil :read-only t)
  (name nil :read-only t)
  (level nil :read-only t)
  (line nil :read-only t)
  (end-line nil :read-only t)
  (node nil :read-only t))

(defun make-outline-entry (&key kind name level line end-line node)
  "Return an outline entry with KIND, NAME, LEVEL, LINE, END-LINE, and NODE."
  (%make-outline-entry :kind kind :name name :level level :line line
                       :end-line end-line :node node))

(defun outline-entry-p (object)
  "Return true when OBJECT is an outline entry."
  (%outline-entry-p object))

(defun outline-entry-kind (entry)
  "Return ENTRY's node kind, such as :HEADLINE or :SRC-BLOCK."
  (%outline-entry-kind entry))

(defun outline-entry-name (entry)
  "Return ENTRY's headline title or block name."
  (%outline-entry-name entry))

(defun outline-entry-level (entry)
  "Return ENTRY's logical headline level, or NIL for a block."
  (%outline-entry-level entry))

(defun outline-entry-line (entry)
  "Return ENTRY's 1-based inclusive start line."
  (%outline-entry-line entry))

(defun outline-entry-end-line (entry)
  "Return ENTRY's 1-based inclusive end line."
  (%outline-entry-end-line entry))

(defun outline-entry-node (entry)
  "Return the parsed node represented by ENTRY."
  (%outline-entry-node entry))

(defstruct (outline-result (:constructor %make-outline-result)
                           (:conc-name %outline-result-)
                           (:copier nil)
                           (:predicate %outline-result-p))
  "The metadata and ordered entries returned by OUTLINE."
  (path nil :read-only t)
  (lang :org :read-only t)
  (symbols nil :read-only t)
  (total 0 :read-only t)
  (truncated nil :read-only t))

(defun make-outline-result (&key path (lang :org) symbols (total 0) truncated)
  "Return an outline result with PATH, LANG, SYMBOLS, TOTAL, and TRUNCATED."
  (%make-outline-result :path path :lang lang :symbols symbols :total total
                        :truncated truncated))

(defun outline-result-p (object)
  "Return true when OBJECT is an outline result."
  (%outline-result-p object))

(defun outline-result-path (result)
  "Return RESULT's document source name, or NIL when it is unset."
  (%outline-result-path result))

(defun outline-result-lang (result)
  "Return RESULT's outline language, currently :ORG."
  (%outline-result-lang result))

(defun outline-result-symbols (result)
  "Return RESULT's outline entries in document order."
  (%outline-result-symbols result))

(defun outline-result-total (result)
  "Return the number of outline entries before applying a limit."
  (%outline-result-total result))

(defun outline-result-truncated (result)
  "Return true when a limit omitted one or more outline entries."
  (%outline-result-truncated result))

(defun outline-block-name (node)
  "Return NODE's affiliated name or its type-specific outline name."
  (let ((name (node-property node :name)))
    (if (not (null name))
        name
        (case (node-type node)
          (:src-block (node-property node :language))
          (:example-block "")))))

(defun outline-entry-for-node (document node)
  "Return an outline entry for NODE when it is a headline or supported block."
  (case (node-type node)
    (:headline
     (make-outline-entry
      :kind :headline
      :name (node-property node :raw-value)
      :level (node-property node :level)
      :line (nth-value 0
                       (offset-line-column document (node-post-affiliated node)))
      :end-line (nth-value 1 (node-line-range node))
      :node node))
    ((:src-block :example-block)
     (multiple-value-bind (line end-line) (node-line-range node)
       (make-outline-entry :kind (node-type node)
                           :name (outline-block-name node)
                           :level nil
                           :line line
                           :end-line end-line
                           :node node)))
    (otherwise nil)))

(defun outline-document-entries (document)
  "Collect supported outline entries from DOCUMENT in preorder."
  (let ((root (document-root document))
        (entries nil))
    (when (not (null root))
      (map-nodes (lambda (node)
                   (let ((entry (outline-entry-for-node document node)))
                     (when (not (null entry))
                       (push entry entries))))
                 root))
    (nreverse entries)))

(defun outline (document &key limit)
  "Return DOCUMENT's ordered headlines and src/example blocks.

LIMIT is NIL for all entries or a non-negative integer. The result records the
unlimited total and is truncated only when LIMIT omits entries."
  (check-type limit (or null (integer 0 *)))
  (let* ((entries (outline-document-entries document))
         (total (length entries))
         (truncated (and limit (> total limit)))
         (symbols (if (not (null truncated))
                      (subseq entries 0 limit)
                      entries)))
    (make-outline-result :path (document-source-name document)
                         :lang :org
                         :symbols symbols
                         :total total
                         :truncated truncated)))
