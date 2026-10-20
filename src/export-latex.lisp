;;;; src/export-latex.lisp
(in-package #:org-kit)

(defstruct (export-latex-state (:constructor %make-export-latex-state))
  preparation
  limits
  render-context
  (references (make-hash-table :test #'equal))
  (reference-ids (make-hash-table :test #'eq))
  (active-footnotes nil))

(defvar *export-latex-render-frame* nil)

(declaim (ftype function export-latex-render-view
                         export-latex-render-object
                         export-latex-join))

(defun export-latex-check-length (state base-count length)
  (check-limit (export-latex-state-limits state) :max-output-length
               (+ base-count length)))

(defun export-latex-error-backend (state)
  (let ((context (export-latex-state-render-context state)))
    (if context
        (export-render-context-backend context)
        :latex)))

(defun export-latex-raw-output-p (state)
  (let ((context (export-latex-state-render-context state)))
    (if context
        (export-render-context-raw-output-p context :latex)
        (export-preparation-allow-raw-latex
         (export-latex-state-preparation state)))))

(defun export-latex-escape-text (state text &optional (base-count 0))
  (with-output-to-string (output)
    (loop with length = 0
          for character across text
          for escaped = (case character
                          (#\# "\\#")
                          (#\$ "\\$")
                          (#\% "\\%")
                          (#\& "\\&")
                          (#\_ "\\_")
                          (#\{ "\\{")
                          (#\} "\\}")
                          (#\~ "\\textasciitilde{}")
                          (#\^ "\\textasciicircum{}")
                          (#\\ "\\textbackslash{}")
                          (otherwise (string character)))
          do (incf length (length escaped))
             (export-latex-check-length state base-count length)
             (write-string escaped output))))

(defun export-latex-source-text (node)
  (or (node-text node) (node-property node :value) ""))

(defun export-latex-reference-slug (value)
  (let ((result
          (with-output-to-string (output)
            (loop with previous-hyphen-p = nil
                  for character across (string-downcase (or value ""))
                  for valid-p = (or (and (< (char-code character) 128)
                                         (alphanumericp character))
                                    (find character ":-"))
                  do (cond
                       (valid-p (write-char character output))
                       ((not previous-hyphen-p) (write-char #\- output)))
                     (setf previous-hyphen-p (char= character #\-))))))
    (string-trim "-" result)))

(defun export-latex-register-reference (state alias id)
  (when (and (stringp alias) (plusp (length alias))
             (not (gethash alias (export-latex-state-references state))))
    (setf (gethash alias (export-latex-state-references state)) id)))

(defun export-latex-index-references (state)
  (let ((used (make-hash-table :test #'equal)))
    (dolist (view (export-collect-views
                   (export-preparation-root-views
                    (export-latex-state-preparation state))))
      (let* ((node (export-view-node view))
             (type (node-type node))
             (custom-id
               (when (member type '(:headline :inlinetask) :test #'eq)
                 (multiple-value-bind (value sources)
                     (effective-property (node-document node) node "CUSTOM_ID")
                   (declare (ignore sources))
                   value)))
             (candidate
               (case type
                 ((:headline :inlinetask)
                  (or custom-id
                      (node-property node :custom-id)
                      (node-property node :raw-value)))
                 ((:target :radio-target) (node-property node :value))))
             (id (and candidate
                      (let* ((base (or (export-latex-reference-slug candidate)
                                       "target"))
                             (suffix (gethash base used 0)))
                        (setf (gethash base used) (1+ suffix))
                        (if (zerop suffix)
                            (concatenate 'string "org:" base)
                            (format nil "org:~A-~D" base (1+ suffix))))))
             (aliases
               (case type
                 ((:headline :inlinetask)
                  (list custom-id
                        (node-property node :custom-id)
                        (node-property node :raw-value)))
                 ((:target :radio-target) (list candidate)))))
        (when id
          (setf (gethash node (export-latex-state-reference-ids state)) id)
          (dolist (alias aliases)
            (export-latex-register-reference state alias id))))))
  state)

(defun export-latex-option (preparation key default)
  (let ((entry (assoc key (export-preparation-options preparation))))
    (if entry (cdr entry) default)))

(defun export-latex-rendered-views (state views base-count &optional inline-p)
  (with-output-to-string (output)
    (loop with length = 0
          with first-p = t
          with previous-view = nil
          for view in views
          for separator = (if first-p
                              ""
                              (if inline-p
                                  (make-string (node-post-blank
                                                (export-view-node previous-view))
                                               :initial-element #\Space)
                                  (if (and (eq (node-type (export-view-node previous-view))
                                               :keyword)
                                           (eq (node-type (export-view-node view))
                                               :keyword))
                                      (string #\Newline)
                                      (format nil "~%~%"))))
          for rendered = (export-latex-render-view
                          state view (+ base-count length (length separator))
                          (if inline-p :inline :block))
          do (when (plusp (length rendered))
               (export-latex-check-length state base-count
                                          (+ length (length separator)
                                             (length rendered)))
               (write-string separator output)
               (write-string rendered output)
               (incf length (+ (length separator) (length rendered)))
               (setf first-p nil
                     previous-view view)))))

(defun export-latex-render-inline-views
    (state views base-count
     &key (table-cell-p
           (and *export-latex-render-frame*
                (export-render-frame-table-cell-p
                 *export-latex-render-frame*)))
       (preserve-source-breaks-p
         (and *export-latex-render-frame*
              (export-render-frame-preserve-source-breaks-p
               *export-latex-render-frame*))))
  (with-output-to-string (output)
    (loop with length = 0
          for tail on views
          for view = (car tail)
          for next-view = (cadr tail)
          for rendered = (export-latex-render-view state view
                                                    (+ base-count length)
                                                    :inline
                                                    :table-cell-p table-cell-p
                                                    :preserve-source-breaks-p
                                                    preserve-source-breaks-p)
          for post-blank = (node-post-blank (export-view-node view))
          for next-node = (and next-view (export-view-node next-view))
          for next-text = (and next-node
                               (member (node-type next-node)
                                       '(:plain-text :text) :test #'eq)
                               (node-text next-node))
          for separator = (if (and (plusp post-blank)
                                   (or (and (plusp (length rendered))
                                            (export-whitespace-character-p
                                             (char rendered (1- (length rendered)))))
                                       (and (stringp next-text)
                                            (plusp (length next-text))
                                            (export-whitespace-character-p
                                             (char next-text 0)))))
                              ""
                              (make-string post-blank :initial-element #\Space))
          for dispatch-owns-spacing-p =
            (export-latex-dispatch-owns-spacing-p state view)
          do (export-latex-check-length state base-count
                                        (+ length (length rendered)
                                           (if dispatch-owns-spacing-p
                                               0
                                               (length separator))))
             (write-string rendered output)
             (unless dispatch-owns-spacing-p
               (write-string separator output))
             (incf length (+ (length rendered)
                             (if dispatch-owns-spacing-p
                                 0
                                 (length separator)))))))

(defun export-latex-render-title (state node view base-count)
  (let ((segments (node-property node :title))
        (title-macros (and view (getf (export-view-payload view) :title-macros))))
    (if segments
        (with-output-to-string (output)
          (loop with length = 0
                for segment in segments
                for macro-entry = (and (node-p segment)
                                       (assoc segment title-macros :test #'eq))
                for rendered =
                  (if (node-p segment)
                      (if macro-entry
                          (export-latex-render-inline-views
                           state (cdr macro-entry) (+ base-count length))
                          (export-latex-render-title-node
                           state segment (+ base-count length)))
                      (export-latex-escape-text state segment
                                               (+ base-count length)))
                for dispatch-owns-spacing-p =
                  (and (node-p segment)
                       (not macro-entry)
                       (export-latex-node-dispatch-owns-spacing-p state segment))
                for spaces = (if (and (node-p segment)
                                      (not dispatch-owns-spacing-p))
                                (make-string (node-post-blank segment)
                                             :initial-element #\Space)
                                "")
                do (export-latex-check-length state base-count
                                              (+ length (length rendered)
                                                 (length spaces)))
                   (write-string rendered output)
                   (write-string spaces output)
                   (incf length (+ (length rendered) (length spaces)))))
        (export-latex-escape-text
         state (or (node-property node :raw-value) "") base-count))))

(defun export-latex-render-heading (state view base-count)
  (let* ((node (export-view-node view))
         (preparation (export-latex-state-preparation state))
         (level (max 1 (or (node-property node :level) 1)))
         (command (nth (min 5 (1- level))
                       '("section" "subsection" "subsubsection"
                         "paragraph" "subparagraph" "subparagraph")))
         (number-limit (export-latex-option preparation :num t))
         (starred-p (or (null number-limit)
                        (and (integerp number-limit)
                             (> level number-limit))))
         (title (export-latex-render-title state node view base-count))
         (todo (and (export-latex-option preparation :todo t)
                    (node-property node :todo-keyword)))
         (priority (and (export-latex-option preparation :priority nil)
                        (node-property node :priority)))
         (tags (and (export-latex-option preparation :tags t)
                    (effective-heading-tags
                     (export-view-source-document view) node :inherit nil)))
         (label (gethash node (export-latex-state-reference-ids state)))
         (children (export-latex-rendered-views
                    state (export-view-children view) base-count)))
    (with-output-to-string (output)
      (let ((length 0))
        (flet ((emit (text)
                 (export-latex-check-length state base-count
                                            (+ length (cl:length text)))
                 (write-string text output)
                 (incf length (cl:length text))))
          (emit (format nil "\\~A~A{" command (if starred-p "*" "")))
          (when todo
            (emit (export-latex-escape-text state todo (+ base-count length)))
            (emit " "))
          (when priority
            (emit "["))
          (when priority
            (emit (export-latex-escape-text
                   state
                   (cond ((characterp priority) (string priority))
                         ((and (integerp priority)
                               (<= (char-code #\A) priority
                                   (char-code #\Z)))
                          (string (code-char priority)))
                         (t (princ-to-string priority)))
                   (+ base-count length)))
            (emit "] "))
          (emit title)
          (when tags
            (emit " \\texttt{[")
            (loop for tag in tags
                  for first-p = t then nil
                  do (unless first-p (emit ":"))
                     (emit (export-latex-escape-text state tag
                                                     (+ base-count length))))
            (emit "]}"))
          (emit "}")
          (when label
            (emit (format nil "\\label{~A}" label)))
          (when (plusp (cl:length children))
            (emit (format nil "~%~%"))
            (emit children))
          )))))

(defun export-latex-render-list-item (state view base-count
                                       &optional descriptive-p)
  (let* ((node (export-view-node view))
         (children (export-view-children view))
         (paragraph (find-if (lambda (child)
                               (eq (node-type (export-view-node child)) :paragraph))
                             children))
         (remaining (if paragraph (remove paragraph children :test #'eq) nil))
         (checkbox (case (node-property node :checkbox)
                     (:on "[X] ") (:off "[ ] ") (:trans "[-] ")))
         (content (if paragraph
                      (export-latex-render-inline-views
                       state (export-view-children paragraph) base-count)
                      (export-latex-rendered-views state children
                                                   base-count))))
    (with-output-to-string (output)
      (write-string "\\item" output)
      (when (and descriptive-p (node-property node :tag))
        (write-string "[{" output)
        (dolist (segment (node-property node :tag))
          (write-string
           (if (node-p segment)
               (export-latex-render-title-node state segment base-count)
               (export-latex-escape-text state segment base-count))
           output))
        (write-string "}]" output))
      (if (and checkbox
               (not (and descriptive-p (node-property node :tag))))
          (write-string "{} " output)
          (write-char #\Space output))
      (when checkbox
        (write-string (export-latex-escape-text state checkbox
                                                base-count) output))
      (write-string content output)
      (dolist (child remaining)
        (let ((rendered (export-latex-render-view state child
                                                  base-count)))
          (when (plusp (length rendered))
            (write-string (format nil "~%~%~A" rendered) output)))))))

(defun export-latex-render-list (state view base-count)
  (let* ((node (export-view-node view))
         (kind (node-property node :type))
         (environment (case kind
                        (:ordered "enumerate")
                        (:descriptive "description")
                        (otherwise "itemize")))
         (children (export-view-children view)))
    (with-output-to-string (output)
      (write-string (format nil "\\begin{~A}" environment) output)
      (loop with length = (+ 8 (length environment))
            for child in children
            for rendered = (export-latex-render-list-item
                            state child (+ base-count length)
                            (eq kind :descriptive))
            for separator = (format nil "~%")
            do (export-latex-check-length state base-count
                                          (+ length (length separator)
                                             (length rendered)))
               (write-string separator output)
               (write-string rendered output)
               (incf length (+ (length separator) (length rendered))))
      (write-string (format nil "~%\\end{~A}" environment) output))))

(defun export-latex-render-table (state view base-count)
  (let* ((node (export-view-node view))
         (rows (export-view-children view))
         (column-count (loop for row in rows
                             maximize (length (export-view-children row))
                               into maximum
                             finally (return (or maximum 0))))
         (columns (make-string column-count :initial-element #\l)))
    (if (eq (node-property node :type) :table.el)
        (export-latex-escape-text state
                                  (or (node-property node :value) "") base-count)
        (with-output-to-string (output)
          (write-string (format nil "\\begin{tabular}{~A}~%" columns) output)
          (loop with length = (+ 18 column-count)
                for row in rows
                for row-node = (export-view-node row)
                for rule-p = (eq (node-property row-node :type) :rule)
                for cells = (export-view-children row)
                for rendered =
                  (if rule-p
                      "\\hline"
                      (with-output-to-string (line)
                        (loop for cell in cells
                              for first-p = t then nil
                              for cell-text =
                                (export-latex-render-inline-views
                                 state (export-view-children cell)
                                 (+ base-count length) :table-cell-p t)
                              do (unless first-p (write-string " & " line))
                                 (write-string cell-text line))))
                for row-text = (if rule-p rendered
                                  (concatenate 'string rendered " \\\\"))
                for separator = (string #\Newline)
                do (export-latex-check-length state base-count
                                              (+ length (length separator)
                                                 (length row-text)))
                   (write-string separator output)
                   (write-string row-text output)
                   (incf length (+ (length separator) (length row-text))))
          (write-string (format nil "~%\\end{tabular}") output)))))

(defun export-latex-expand-link-abbreviation (raw document)
  (link-object-expand-abbreviation document raw))

(defun export-latex-link-source (node)
  (let ((raw (node-property node :raw-link))
        (type (node-property node :type))
        (path (node-property node :path)))
    (or raw
        (if (and (stringp type) (stringp path)
                 (not (member type '("fuzzy" "custom-id" "radio")
                               :test #'string-equal)))
            (concatenate 'string type ":" path)
            (or path "")))))

(defun export-latex-link-label (state view base-count)
  (let ((children (export-view-children view)))
    (if children
        (export-latex-render-inline-views state children base-count)
        (export-latex-escape-text
         state (or (node-property (export-view-node view) :raw-link)
                   (node-property (export-view-node view) :path) "")
         base-count))))

(defun export-latex-image-path-p (path)
  (export-image-path-p path '("png" "jpg" "jpeg" "gif" "svg" "pdf")))

(defun export-latex-internal-link-id (state path)
  (let* ((clean-path (if (and (stringp path) (plusp (length path))
                           (char= (char path 0) #\#))
                      (subseq path 1) path))
         (references (export-latex-state-references state)))
    (gethash clean-path references)))

(defun export-latex-render-link (state view base-count &optional frame)
  (let* ((node (export-view-node view))
         (type (node-property node :type))
         (path (node-property node :path))
         (label (export-latex-link-label state view base-count))
         (context (export-latex-state-render-context state))
         (link-result
           (and context frame
                (export-call-link-type-handler-for-view
                 context node view frame))))
    (if link-result
        (export-latex-render-link-result state link-result path label base-count)
      (cond
        ((and (stringp type) (stringp path)
              (member type '("fuzzy" "custom-id" "radio")
                      :test #'string-equal))
         (let ((id (export-latex-internal-link-id state path)))
           (if id (format nil "\\hyperref[~A]{~A}" id label) label)))
        ((and (stringp path)
              (string-equal (or type "") "file"))
         (let ((expanded
                 (export-latex-expand-link-abbreviation
                  path (export-preparation-document
                        (export-latex-state-preparation state)))))
           (cond
             ((export-latex-image-path-p expanded)
              (if (export-safe-relative-file-path-p expanded)
                  (format nil "\\includegraphics{~A}" expanded)
                  label))
             ((export-safe-url-p path expanded)
              (format nil "\\href{~A}{~A}"
                      (export-latex-escape-text state expanded base-count)
                      label))
             (t label))))
        ((and (stringp type) (stringp path))
         (let* ((raw (export-latex-link-source node))
                (expanded
                  (export-latex-expand-link-abbreviation
                   raw (export-preparation-document
                        (export-latex-state-preparation state)))))
           (if (and (export-safe-url-p raw expanded)
                    (member type '("http" "https" "mailto")
                            :test #'string-equal))
               (format nil "\\href{~A}{~A}"
                       (export-latex-escape-text state expanded base-count)
                       label)
               label)))
        (t label)))))

(defun export-latex-render-link-result (state result path label base-count)
  (let ((destination (export-link-result-destination result))
        (description (export-link-result-description result)))
    (case (export-link-result-kind result)
      (:link
       (if (export-safe-url-p path destination)
           (format nil "\\href{~A}{~A}"
                   (export-latex-escape-text state destination base-count)
                   (export-latex-escape-text state description base-count))
           label))
      (:image
       (let ((scheme-end (export-explicit-url-scheme-end destination)))
         (cond
           ((and scheme-end
                 (export-allowed-url-scheme-p destination scheme-end)
                 (member (subseq destination 0 scheme-end) '("http" "https")
                         :test #'string-equal)
                 (export-safe-url-p path destination))
            (format nil "\\includegraphics{~A}"
                    (export-latex-escape-text state destination base-count)))
           ((and (null scheme-end)
                 (export-safe-relative-file-path-p destination))
            (format nil "\\includegraphics{~A}"
                    (export-latex-escape-text state destination base-count)))
           (t label)))))))

(defun export-latex-footnote-entry (label preparation)
  (find label (export-preparation-footnotes preparation)
        :key (lambda (entry) (getf entry :label)) :test #'string=))

(defun export-latex-view-for-node (node &optional prune-children-p)
  (%make-export-view
   :node node
   :children (unless prune-children-p
              (mapcar #'export-latex-view-for-node (node-children node)))
   :source-document (node-document node)))

(defun export-latex-node-dispatch-owns-spacing-p (state node)
  (let ((context (export-latex-state-render-context state)))
    (when context
      (let ((ancestry (org-kit::%export-render-context-ancestry context)))
        (multiple-value-bind (transcoder transcoder-owner)
            (export-resolved-map-value
             ancestry #'%export-backend-descriptor-transcoders (node-type node))
          (declare (ignore transcoder))
          (multiple-value-bind (wrapper wrapper-owner)
              (export-resolved-component
               ancestry #'%export-backend-descriptor-node-wrapper)
            (declare (ignore wrapper))
            (export-render-node-scratch-required-p
             ancestry (node-type node) transcoder-owner wrapper-owner)))))))

(defun export-latex-dispatch-owns-spacing-p (state view)
  (export-latex-node-dispatch-owns-spacing-p
   state (export-view-node view)))

(defun export-latex-strip-block-post-blank (view rendered)
  (export-node-strip-post-blank (export-view-node view) rendered))

(defun export-latex-render-title-node (state node base-count)
  (let* ((context (export-latex-state-render-context state))
         (policy (and context
                      (export-resolved-node-policy
                       (org-kit::%export-render-context-ancestry context)
                       (node-type node))))
         (view (export-latex-view-for-node node
                                           (eq policy :prune-children))))
    (if context
        (export-latex-render-view state view base-count :inline)
        (export-latex-render-object state node base-count view))))

(defun export-latex-render-footnote (state view base-count)
  (let* ((node (export-view-node view))
         (label (node-property node :label))
         (entry (and label
                     (export-latex-footnote-entry
                      label (export-latex-state-preparation state))))
         (active (export-latex-state-active-footnotes state)))
    (unless (or entry (null label))
      (export-error (export-latex-error-backend state) :unresolved-footnote node
                    "Footnote reference has no selected definition"))
    (check-limit (export-latex-state-limits state)
                 :max-export-expansion-depth (length active))
    (when (and label (member label active :test #'equal))
      (return-from export-latex-render-footnote "\\footnotemark"))
    (when active
      (return-from export-latex-render-footnote "\\footnotemark"))
    (setf (export-latex-state-active-footnotes state)
          (cons (or label :inline) active))
    (unwind-protect
         (let ((content
                 (if entry
                     (let* ((definition (getf entry :definition))
                            (children (export-view-children definition)))
                       (export-latex-rendered-views state children base-count))
                     (export-latex-render-inline-views
                      state (export-view-children view) base-count))))
           (format nil "\\footnote{~A}" content))
      (setf (export-latex-state-active-footnotes state) active))))

(defun export-latex-render-code (state value base-count &optional body-end)
  (let* ((text (or value ""))
         (end (if body-end body-end (length text)))
         (wrapper-length
           (length (format nil "\\begin{verbatim}~%~%\\end{verbatim}"))))
    (loop while (and (plusp end) (char= (char text (1- end)) #\Newline))
          do (decf end))
    (when (search "\\end{verbatim}" text :end2 end)
      (export-error (export-latex-error-backend state) :unsafe-verbatim-body nil
                    "Code body contains the verbatim closing delimiter"))
    (export-latex-check-length state base-count (+ wrapper-length end))
    (format nil "\\begin{verbatim}~%~A~%\\end{verbatim}"
            (subseq text 0 end))))

(defun export-latex-render-attr-comment (state value base-count)
  (let ((prefix "% ATTR_LATEX: "))
    (export-latex-check-length state base-count
                               (+ (length prefix) (length value)))
    (concatenate 'string prefix value)))

(defun export-latex-render-source-block (state view base-count)
  (let* ((node (export-view-node view))
         (mode (getf (export-view-payload view) :requested-exports))
         (source (or (node-property node :value) ""))
         (source-p (not (member mode '(:none :results) :test #'eq)))
         (results-p (not (member mode '(:none :code) :test #'eq)))
         (results (and results-p
                       (export-latex-rendered-views
                        state (export-view-children view) base-count)))
         (source-end (length source)))
    (loop while (and (plusp source-end)
                     (char= (char source (1- source-end)) #\Newline))
          do (decf source-end))
    (export-latex-join
     state base-count
     (remove-if (lambda (part) (or (null part) (zerop (length part))))
                (list (and source-p (plusp source-end)
                           (export-latex-render-code state source base-count
                                                     source-end))
                      results))
     (format nil "~%~%"))))

(defun export-latex-join (state base-count strings separator)
  (with-output-to-string (output)
    (loop with length = 0
          with first-p = t
          for string in strings
          when (and (stringp string) (plusp (length string)))
            do (let ((gap (if first-p "" separator)))
                 (export-latex-check-length
                  state base-count (+ length (length gap) (length string)))
                 (write-string gap output)
                 (write-string string output)
                 (incf length (+ (length gap) (length string)))
                 (setf first-p nil)))))

(defun export-latex-render-object (state node base-count &optional view)
  (let* ((type (node-type node))
         (children (if view (export-view-children view) (node-children node)))
         (child-views (if view children
                          (mapcar #'export-latex-view-for-node children)))
         (content (lambda ()
                    (export-latex-render-inline-views
                     state child-views base-count))))
    (case type
      (:bold (format nil "\\textbf{~A}" (funcall content)))
      (:italic (format nil "\\emph{~A}" (funcall content)))
      (:strike-through (format nil "\\sout{~A}" (funcall content)))
      (:underline (format nil "\\uline{~A}" (funcall content)))
      ((:subscript :superscript)
       (format nil (if (eq type :subscript) "\\textsubscript{~A}"
                       "\\textsuperscript{~A}")
               (funcall content)))
      ((:code :verbatim)
       (let* ((source (export-latex-source-text node))
              (wrapped-p (and (> (length source) 1)
                              (member (char source 0) '(#\~ #\=))
                              (char= (char source 0)
                                     (char source (1- (length source))))))
              (start (if wrapped-p 1 0))
              (end (if wrapped-p (1- (length source)) (length source))))
         (format nil "\\texttt{~A}"
                 (export-latex-escape-text state (subseq source start end)
                                           base-count))))
      (:entity (or (node-property node :latex)
                   (export-latex-escape-text state
                                             (or (node-property node :utf-8)
                                                 (node-property node :unicode)
                                                 (node-text node) "") base-count)))
      (:latex-fragment
       (if (export-latex-raw-output-p state)
           (export-latex-source-text node) ""))
      (:export-snippet
       (if (and (export-latex-raw-output-p state)
                (string-equal (or (node-property node :back-end) "") "latex"))
           (or (node-property node :value) "") ""))
      (:footnote-reference
       (export-latex-render-footnote
        state (or view (export-latex-view-for-node node)) base-count))
      (:inline-babel-call
       (export-latex-escape-text state (export-latex-source-text node) base-count))
      (:inline-src-block
       (format nil "\\texttt{~A}"
               (export-latex-escape-text state (or (node-property node :value) "")
                                         base-count)))
      (:line-break (format nil "\\\\{}~%"))
      (:link (export-latex-render-link
              state (or view (export-latex-view-for-node node)) base-count
              *export-latex-render-frame*))
      (:macro (funcall content))
      ((:plain-text :text)
       (export-latex-escape-text state (or (node-text node) "") base-count))
      (:radio-target
       (let ((id (gethash node (export-latex-state-reference-ids state)))
             (label (if children
                        (funcall content)
                        (export-latex-escape-text
                         state (or (node-property node :value) "")
                         base-count))))
         (export-latex-join
          state base-count
          (list (and id (format nil "\\label{~A}" id)) label)
          "")))
      (:target
       (let ((id (gethash node (export-latex-state-reference-ids state))))
         (if id (format nil "\\label{~A}" id) "")))
      (:statistics-cookie
       (export-latex-escape-text state (export-latex-source-text node) base-count))
      ((:citation :citation-reference)
       (export-latex-escape-text state (export-latex-source-text node) base-count))
      (:timestamp
       (export-latex-escape-text state (export-latex-source-text node) base-count))
      (otherwise (funcall content)))))

(defun export-latex-render-view-legacy (state view &optional (base-count 0))
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view))
         (rendered
           (case type
             ((:org-data :section :drawer)
              (export-latex-rendered-views state children base-count))
             ((:headline :inlinetask)
              (export-latex-render-heading state view base-count))
             (:paragraph
              (export-latex-render-inline-views state children base-count))
             (:macro
              (export-latex-rendered-views state children base-count))
             (:center-block
              (format nil "\\begin{center}~%~A~%\\end{center}"
                      (export-latex-rendered-views state children base-count)))
             (:quote-block
              (format nil "\\begin{quote}~%~A~%\\end{quote}"
                      (export-latex-rendered-views state children base-count)))
             (:special-block
              (let* ((environment (or (node-property node :type) "specialblock"))
                     (content (export-latex-rendered-views state children base-count))
                     (valid-environment-p
                       (and (stringp environment)
                            (plusp (length environment))
                            (alpha-char-p (char environment 0))
                            (loop for character across environment
                                  for index from 0
                                  always
                                  (or (and (< (char-code character) 128)
                                           (alpha-char-p character))
                                      (and (char= character #\*)
                                           (= index (1- (length environment)))))))))
                (if valid-environment-p
                    (format nil "\\begin{~A}~%~A~%\\end{~A}"
                            environment content environment)
                    (format nil "\\begin{quote}~%~A~%\\end{quote}"
                            content))))
             ((:property-drawer :node-property :planning :clock :comment
               :comment-block :footnote-definition)
              "")
             (:keyword
              (let ((key (string-upcase (or (node-property node :key) "")))
                    (value (or (node-property node :value) "")))
                (cond ((string= key "LATEX")
                       (if (export-latex-raw-output-p state)
                           value ""))
                      ((string= key "ATTR_LATEX")
                       (export-latex-render-attr-comment
                        state value base-count))
                      (t ""))))
             (:dynamic-block
              (export-latex-render-code state
                                        (or (node-property node :value)
                                            (node-text node) "") base-count))
             (:plain-list (export-latex-render-list state view base-count))
             (:item (export-latex-render-list-item state view base-count))
             (:table (export-latex-render-table state view base-count))
             (:table-row
              (export-latex-join
               state base-count
               (loop for cell in children
                     collect (export-latex-render-inline-views
                              state (export-view-children cell) base-count
                              :table-cell-p t))
               " & "))
             (:table-cell
              (export-latex-render-inline-views state children base-count
                                                :table-cell-p t))
             ((:babel-call :inline-babel-call :diary-sexp)
              (export-latex-escape-text state (export-latex-source-text node)
                                        base-count))
             ((:example-block :fixed-width)
              (export-latex-render-code state
                                        (or (node-property node :value)
                                            (node-text node) "") base-count))
             (:src-block (export-latex-render-source-block state view base-count))
             (:export-block
              (if (and (export-latex-raw-output-p state)
                       (member (or (node-property node :type) "")
                               '("latex" "tex") :test #'string-equal))
                  (or (node-property node :value) "") ""))
             (:horizontal-rule "\\noindent\\rule{\\linewidth}{0.4pt}")
             (:latex-environment
              (if (export-latex-raw-output-p state)
                  (export-latex-source-text node) ""))
             (:verse-block
              (format nil "\\begin{verse}~%~A~%\\end{verse}"
                      (export-latex-rendered-views state children base-count)))
             (otherwise
              (export-latex-render-object state node base-count view))))
         (attribute (and (null (export-latex-state-render-context state))
                         (node-property node :ATTR_LATEX))))
    (let* ((attribute-comment
             (and (stringp attribute)
                  (export-latex-render-attr-comment
                   state attribute base-count)))
           (result
             (if attribute-comment
                 (progn
                   (export-latex-check-length
                    state base-count
                    (+ (length attribute-comment) 1 (length rendered)))
                   (concatenate 'string attribute-comment
                                (string #\Newline) rendered))
                 rendered)))
      (unless (export-latex-state-render-context state)
        (export-latex-check-length state base-count (length result)))
      result)))

(defun export-latex-render-view (state view &rest arguments)
  (let* ((base-count (if (and arguments (not (keywordp (first arguments))))
                         (pop arguments)
                         0))
         (mode (if (and arguments
                        (member (first arguments) (list :block :inline :object)))
                   (pop arguments)
                   :block)))
    (unless (loop for tail = arguments then (cddr tail)
                  while tail
                  always (and (cdr tail)
                              (member (car tail)
                                     (list :table-cell-p
                                           :preserve-source-breaks-p))))
      (error "Invalid LaTeX render arguments"))
    (let* ((table-cell-p
             (getf arguments :table-cell-p
                   (and *export-latex-render-frame*
                        (export-render-frame-table-cell-p
                         *export-latex-render-frame*))))
           (preserve-source-breaks-p
             (getf arguments :preserve-source-breaks-p
                   (and *export-latex-render-frame*
                        (export-render-frame-preserve-source-breaks-p
                         *export-latex-render-frame*))))
           (context (export-latex-state-render-context state)))
      (if context
          (let* ((*export-current-output-offset* base-count)
                 (node (export-view-node view))
                 (result (export-render-view
                          context view :mode mode :table-cell-p table-cell-p
                          :preserve-source-breaks-p preserve-source-breaks-p))
                 (post-blank (node-post-blank node)))
            (cond
              ((eq mode :block)
               (export-latex-strip-block-post-blank view result))
              ((and (plusp post-blank)
                    (not (export-latex-node-dispatch-owns-spacing-p state node)))
               (subseq result 0 (- (length result) post-blank)))
              (t result)))
          (export-latex-render-view-legacy state view base-count)))))

(defun export-latex-keyword-values (document key)
  (loop for (node keyword value) in (export-keyword-records document)
        when (string= keyword key)
          collect (list node value)))

(defun export-latex-document-class (state)
  (let ((context (export-latex-state-render-context state)))
    (when context
      (loop for descriptor in
            (reverse (org-kit::%export-render-context-ancestry context))
            for class = (%export-backend-descriptor-document-class descriptor)
            when class return class))))

(defun export-latex-preamble (state)
  (let* ((preparation (export-latex-state-preparation state))
         (document (export-preparation-document preparation))
         (records (export-keyword-records document))
         (allow-raw-latex (export-latex-raw-output-p state))
         (class (and allow-raw-latex
                     (nth-value 0
                       (export-single-keyword-value records "LATEX_CLASS" :latex))))
         (class-options
           (and allow-raw-latex
                (nth-value 0
                  (export-single-keyword-value
                   records "LATEX_CLASS_OPTIONS" :latex))))
         (headers (and allow-raw-latex
                       (append
                        (export-latex-keyword-values document "LATEX_HEADER")
                        (export-latex-keyword-values document "LATEX_HEADER_EXTRA"))))
         (document-class (export-latex-document-class state))
         (metadata (export-preparation-metadata preparation))
         (title (getf metadata :title))
         (author (getf metadata :author))
         (date (getf metadata :date))
         (title-enabled (export-latex-option preparation :title t)))
    (with-output-to-string (output)
      (let ((emitted-length 0))
        (labels ((emit (value)
                   (export-latex-check-length
                    state 0 (+ emitted-length (length value)))
                   (write-string value output)
                   (incf emitted-length (length value))))
          (let* ((class-options-text
                   (if (and (stringp class-options)
                            (plusp (length class-options)))
                       (if (char= (char class-options 0) #\[)
                           class-options
                           (format nil "[~A]" class-options))
                       "[11pt]"))
                 (class-text (or class document-class "article")))
            (export-latex-check-length
             state emitted-length
             (+ (length "\\documentclass") (length class-options-text)
                (length "{") (length class-text) (length "}") 1))
            (emit (format nil "\\documentclass~A{~A}~%"
                          class-options-text class-text)))
          (emit (format nil "\\usepackage[utf8]{inputenc}~%\\usepackage[T1]{fontenc}~%\\usepackage[normalem]{ulem}~%\\usepackage{graphicx}~%\\usepackage{hyperref}~%"))
          (dolist (entry headers)
            (let ((header (second entry)))
              (emit header)
              (unless (and (plusp (length header))
                           (char= (char header (1- (length header))) #\Newline))
                (emit (string #\Newline)))))
          (emit (string #\Newline))
          (when title
            (let ((escaped (export-latex-escape-text state title 0)))
              (export-latex-check-length
               state emitted-length
               (+ (length "\\title{}") 1 (length escaped)))
              (emit (format nil "\\title{~A}~%" escaped))))
          (when author
            (let ((escaped (export-latex-escape-text state author 0)))
              (export-latex-check-length
               state emitted-length
               (+ (length "\\author{}") 1 (length escaped)))
              (emit (format nil "\\author{~A}~%" escaped))))
          (let ((escaped (export-latex-escape-text state (or date "") 0)))
            (export-latex-check-length
             state emitted-length
             (+ (length (format nil "\\date{}~%~%\\begin{document}~%"))
                (length escaped)))
            (emit (format nil "\\date{~A}~%~%\\begin{document}~%" escaped)))
          (when (and title title-enabled)
            (emit (format nil "\\maketitle~%")))
          (when (export-preparation-toc preparation)
            (emit (format nil "\\tableofcontents~%~%"))))))))

(defun export-latex-render-document (state)
  (let* ((preparation (export-latex-state-preparation state))
         (preamble (export-latex-preamble state))
         (body (export-latex-rendered-views
                state (export-preparation-root-views preparation)
                (length preamble)))
         (suffix (format nil "~%~%\\end{document}~%")))
    (export-latex-check-length state 0
                               (+ (length preamble) (length body)
                                  (length suffix)))
    (concatenate 'string preamble body suffix)))

(defun export-latex-make-state (context)
  (let* ((preparation (org-kit::%export-render-context-preparation context))
         (state (%make-export-latex-state
                 :preparation preparation
                 :limits (export-render-context-limits context))))
    (export-latex-index-references state)
    state))

(defun export-latex-set-render-context (context)
  (let ((state (export-render-context-state context)))
    (setf (export-latex-state-preparation state)
          (org-kit::%export-render-context-preparation context)
          (export-latex-state-limits state) (export-render-context-limits context)
          (export-latex-state-render-context state) context)
    state))

(defun export-latex-render-preamble (context)
  (export-latex-preamble (export-latex-set-render-context context)))

(defun export-latex-render-body (context)
  (let* ((state (export-latex-set-render-context context))
         (preparation (export-latex-state-preparation state))
         (views (export-preparation-root-views preparation))
         (ancestry (org-kit::%export-render-context-ancestry context))
         (legacy-path-p
           (and (eq (export-render-context-backend context) :latex)
                (= (length ancestry) 1)
                (null (org-kit::%export-backend-descriptor-filters
                       (first ancestry)))
                (null (org-kit::%export-backend-descriptor-link-type-handlers
                       (first ancestry))))))
    (if legacy-path-p
        (let ((legacy-state
                (%make-export-latex-state
                 :preparation preparation
                 :limits (export-render-context-limits context))))
          (export-latex-index-references legacy-state)
          (export-latex-rendered-views
           legacy-state views *export-current-output-offset*))
        (export-latex-rendered-views
         state views *export-current-output-offset*))))

(defun export-latex-render-trailer (context)
  (declare (ignore context))
  (format nil "~%~%\\end{document}~%"))

(defun export-latex-transcode-node (node view frame context)
  (let* ((state (export-latex-set-render-context context))
         (base-count (export-render-frame-output-offset frame))
         (*export-current-output-offset* base-count)
         (*export-latex-render-frame* frame)
         (active-view (or view (export-latex-view-for-node node))))
    (export-latex-render-view-legacy state active-view base-count)))

(defun export-latex-node-transcoders ()
  (mapcar (lambda (type)
            (cons type #'export-latex-transcode-node))
          (node-types)))

(defun export-latex-node-wrapper (node view frame context render-next)
  (declare (ignore view))
  (let* ((state (export-latex-set-render-context context))
         (attribute (node-property node :ATTR_LATEX))
         (prefix (if (stringp attribute)
                     (concatenate
                      'string
                      (export-latex-render-attr-comment
                       state attribute
                       (export-render-frame-output-offset frame))
                      (string #\Newline))
                     "")))
    (funcall render-next prefix ""))
  nil)

(eval-when (:load-toplevel :execute)
  (%register-builtin-export-backend
   :latex
   :renderer #'export-latex-render-body
   :preamble-renderer #'export-latex-render-preamble
   :trailer-renderer #'export-latex-render-trailer
   :make-state #'export-latex-make-state
   :document-class "article"
   :transcoders (export-latex-node-transcoders)
   :node-wrapper #'export-latex-node-wrapper
   :accepted-keywords
   '("LATEX_CLASS" "LATEX_CLASS_OPTIONS" "LATEX_HEADER"
     "LATEX_HEADER_EXTRA" "LATEX" "ATTR_LATEX")
   :retained-keywords '("LATEX" "ATTR_LATEX")
   :raw-output-options '((:latex . :allow-raw-latex))
   :export-block-names '("latex" "tex")
   :export-snippet-names '("latex")))
