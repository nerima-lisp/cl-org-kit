;;;; src/export-gfm.lisp
(in-package #:org-kit)

(defstruct (export-gfm-state (:constructor %make-export-gfm-state))
  preparation
  limits
  render-context
  render-frame
  (output-offset 0)
  (indexed-p nil)
  heading-ids
  heading-targets
  anchored-headings
  reference-ids
  used-ids
  footnote-reference-counts)

(defun export-gfm-state-for-preparation (preparation)
  (let ((document (export-preparation-document preparation)))
    (%make-export-gfm-state
     :preparation preparation
     :limits (and document (document-limits document))
     :heading-ids (make-hash-table :test #'eq)
     :heading-targets (make-hash-table :test #'equal)
     :anchored-headings (make-hash-table :test #'eq)
     :reference-ids (make-hash-table :test #'equalp)
     :used-ids (make-hash-table :test #'equalp)
     :footnote-reference-counts (make-hash-table :test #'equal))))

(defun export-gfm-make-state (context)
  (export-gfm-state-for-preparation
   (org-kit::%export-render-context-preparation context)))

(defvar *export-gfm-output-limits* nil)
(defvar *export-gfm-output-prefix-length* 0)
(progn
(defvar *export-gfm-table-column-count* nil)
(defvar *export-gfm-render-footnote-definition-body-p* nil))
(defvar *export-gfm-list-marker* nil)
(defvar *export-gfm-list-depth* 0)

(defun export-gfm-check-total-output-length (length)
  (when *export-gfm-output-limits*
    (export-check-output-actual
     (+ *export-gfm-output-prefix-length* length)
     *export-gfm-output-limits* :gfm :body))
  length)

(defun export-gfm-render-with-prefix (prefix-length function)
  (let ((outer-prefix *export-gfm-output-prefix-length*))
    (let* ((*export-gfm-output-prefix-length* (+ outer-prefix prefix-length))
           (rendered (funcall function)))
      (let ((*export-gfm-output-prefix-length* outer-prefix))
        (export-gfm-check-total-output-length
         (+ prefix-length (length rendered))))
      rendered)))

(defun export-gfm-normalized-newline-length (text &optional (start 0) end)
  (let ((end (or end (length text))))
    (- (- end start)
       (loop for index from start below (1- end)
             count (and (char= (char text index) #\Return)
                        (char= (char text (1+ index)) #\Newline))))))

(defun export-gfm-ascii-punctuation-p (character)
  (let ((code (char-code character)))
    (or (<= #x21 code #x2f)
        (<= #x3a code #x40)
        (<= #x5b code #x60)
        (<= #x7b code #x7e))))

(defun export-gfm-normalize-newlines (text &optional (start 0) end)
  (let ((end (or end (length text))))
  (export-gfm-check-total-output-length
   (export-gfm-normalized-newline-length text start end))
  (with-output-to-string (output)
    (loop with index = start
          while (< index end)
          for character = (char text index)
          do (cond
               ((char= character #\Return)
                (write-char #\Newline output)
                (when (and (< (1+ index) end)
                           (char= (char text (1+ index)) #\Newline))
                  (incf index)))
               (t (write-char character output)))
             (incf index)))))

(defun export-gfm-loop-escaped-literal-parts
    (parts emit &key table-cell-p soft-newline-p trim-terminal-newline-p)
  (let ((remaining parts)
        (source nil)
        (index 0)
        (end 0))
    (labels ((available-p ()
               (loop while (and (or (null source) (>= index end)) remaining)
                     do (destructuring-bind (next-source next-start next-end)
                            (pop remaining)
                          (setf source next-source
                                index next-start
                                end next-end)))
               (and source (< index end)))
             (peek ()
               (when (available-p)
                 (char source index)))
             (consume ()
               (let ((character (peek)))
                 (when character (incf index))
                 character))
             (newline-p (character)
               (member character '(#\Return #\Newline) :test #'char=))
             (range-terminal-newlines-p (text start finish)
               (loop for position from start below finish
                     always (newline-p (char text position))))
             (remaining-terminal-newlines-p ()
               (and (range-terminal-newlines-p source index end)
                    (loop for (text start finish) in remaining
                          always
                          (range-terminal-newlines-p text start finish)))))
      (loop while (available-p)
            for character = (char source index)
            do (cond
                 ((char= character #\Tab)
                  (funcall emit #\Space)
                  (consume))
                 ((newline-p character)
                  (cond
                    (table-cell-p
                     (funcall emit "<br>")
                     (consume)
                     (when (and (char= character #\Return)
                                (eql (peek) #\Newline))
                       (consume)))
                    ((and trim-terminal-newline-p
                          (remaining-terminal-newlines-p))
                     (setf remaining nil index end))
                    (soft-newline-p
                     (loop while (and (available-p)
                                      (newline-p (char source index)))
                           for newline = (consume)
                           do (when (and (char= newline #\Return)
                                         (eql (peek) #\Newline))
                                (consume)))
                     (when (available-p)
                       (funcall emit #\Space)))
                    (t
                     (funcall emit #\Newline)
                     (consume)
                     (when (and (char= character #\Return)
                                (eql (peek) #\Newline))
                       (consume)))))
                 ((export-gfm-ascii-punctuation-p character)
                  (funcall emit "\\")
                  (funcall emit character)
                  (consume))
                 (t
                  (funcall emit character)
                  (consume))))))
  nil)

(defun export-gfm-escaped-literal-parts-output-length
    (parts &key table-cell-p soft-newline-p trim-terminal-newline-p)
  (let ((output-length 0))
    (export-gfm-loop-escaped-literal-parts
     parts (lambda (item)
             (incf output-length
                   (if (characterp item) 1 (length item))))
     :table-cell-p table-cell-p :soft-newline-p soft-newline-p
     :trim-terminal-newline-p trim-terminal-newline-p)
    output-length))

(defun export-gfm-escaped-literal-output-length
    (text &key table-cell-p soft-newline-p trim-terminal-newline-p
                start end)
  (export-gfm-escaped-literal-parts-output-length
   (list (list text (or start 0) (or end (length text))))
   :table-cell-p table-cell-p :soft-newline-p soft-newline-p
   :trim-terminal-newline-p trim-terminal-newline-p))

(defun export-gfm-escape-literal-parts
    (parts &key table-cell-p soft-newline-p trim-terminal-newline-p)
  (export-gfm-check-total-output-length
   (export-gfm-escaped-literal-parts-output-length
    parts :table-cell-p table-cell-p :soft-newline-p soft-newline-p
    :trim-terminal-newline-p trim-terminal-newline-p))
  (with-output-to-string (output)
    (export-gfm-loop-escaped-literal-parts
     parts (lambda (item)
             (if (characterp item)
                 (write-char item output)
                 (write-string item output)))
     :table-cell-p table-cell-p :soft-newline-p soft-newline-p
     :trim-terminal-newline-p trim-terminal-newline-p)))

(defun export-gfm-escape-literal-range
    (text start end &key table-cell-p soft-newline-p trim-terminal-newline-p)
  (export-gfm-escape-literal-parts
   (list (list text start end)) :table-cell-p table-cell-p
   :soft-newline-p soft-newline-p
   :trim-terminal-newline-p trim-terminal-newline-p))

(defun export-gfm-escape-literal
    (text &key table-cell-p soft-newline-p trim-terminal-newline-p)
  (export-gfm-escape-literal-range
   text 0 (length text) :table-cell-p table-cell-p
   :soft-newline-p soft-newline-p
   :trim-terminal-newline-p trim-terminal-newline-p))

(defun export-gfm-escape-node-source-text
    (node &key table-cell-p soft-newline-p trim-terminal-newline-p)
  (let* ((document (node-document node))
         (start (node-begin node))
         (end (node-end node)))
    (if (and document start end)
        (export-gfm-escape-literal-range
         (document-text document) start end
         :table-cell-p table-cell-p :soft-newline-p soft-newline-p
         :trim-terminal-newline-p trim-terminal-newline-p)
        (export-gfm-escape-literal
         (or (node-text node) "")
         :table-cell-p table-cell-p :soft-newline-p soft-newline-p
         :trim-terminal-newline-p trim-terminal-newline-p))))

(defun export-gfm-html-attribute-output-length (text)
  (loop for character across text
        sum (case character
              (#\& 5)
              ((#\< #\>) 4)
              (#\" 6)
              (t 1))))

(defun export-gfm-escape-html-attribute (text)
  (export-gfm-check-total-output-length
   (export-gfm-html-attribute-output-length text))
  (with-output-to-string (output)
    (loop for character across text
          do (case character
               (#\& (write-string "&amp;" output))
               (#\< (write-string "&lt;" output))
               (#\> (write-string "&gt;" output))
               (#\" (write-string "&quot;" output))
               (t (write-char character output))))))

(defun export-gfm-longest-run (text character &optional (start 0) end)
  (let ((end (or end (length text))))
  (loop with longest = 0
        with current = 0
        for index from start below end
        for item = (char text index)
        do (if (char= item character)
               (setf longest (max longest (incf current)))
               (setf current 0))
        finally (return longest))))

(defun export-gfm-code-span (value &key table-cell-p start end)
  (let* ((start (or start 0))
         (end (or end (length value)))
         (normalized-length
           (export-gfm-normalized-newline-length value start end))
         (pipe-count (if table-cell-p
                         (loop for index from start below end
                               count (char= (char value index) #\|))
                         0))
         (delimiter-length
           (1+ (export-gfm-longest-run value #\` start end)))
         (pad-p (and (plusp normalized-length)
                     (or (find (char value start)
                               '(#\Space #\Tab #\` #\Return #\Newline))
                         (find (char value (1- end))
                               '(#\Space #\Tab #\` #\Return #\Newline)))))
         (output-length (+ (* 2 delimiter-length) normalized-length pipe-count
                           (if pad-p 2 0))))
    (export-gfm-check-total-output-length output-length)
    (let* ((normalized-text (substitute #\Space #\Newline
                                        (export-gfm-normalize-newlines
                                         value start end)))
           (text (if (and table-cell-p (find #\| normalized-text))
                     (with-output-to-string (output)
                       (loop for character across normalized-text
                             do (when (char= character #\|)
                                  (write-char #\\ output))
                                (write-char character output)))
                     normalized-text))
           (delimiter (make-string delimiter-length :initial-element #\`))
           (actual-pad-p (and (plusp (length text))
                              (or (find (char text 0) '(#\Space #\Tab #\`))
                                  (find (char text (1- (length text)))
                                        '(#\Space #\Tab #\`))))))
      (if actual-pad-p
          (format nil "~A ~A ~A" delimiter text delimiter)
          (format nil "~A~A~A" delimiter text delimiter)))))

(defun export-gfm-code-fence (value)
  (make-string (max 3 (1+ (export-gfm-longest-run value #\~)))
               :initial-element #\~))

(defun export-gfm-source-language (node)
  (let ((language (node-property node :language)))
    (when (and (stringp language)
               (plusp (length language))
               (every (lambda (character)
                        (or (alphanumericp character)
                            (find character "+-_")))
                      language))
      language)))

(defun export-gfm-fenced-block (value &optional language)
  (let* ((body-length (export-gfm-normalized-newline-length value))
         (fence-length (max 3 (1+ (export-gfm-longest-run value #\~))))
         (body-ends-in-newline-p
           (and (plusp (length value))
                (member (char value (1- (length value)))
                        '(#\Return #\Newline) :test #'char=)))
         (output-length (+ (* 2 fence-length) (length (or language "")) 1
                           body-length
                           (if (and (plusp body-length)
                                    (not body-ends-in-newline-p))
                               1 0))))
    (export-gfm-check-total-output-length output-length)
    (let* ((body (export-gfm-normalize-newlines value))
           (fence (make-string fence-length :initial-element #\~)))
      (with-output-to-string (output)
        (write-string fence output)
        (when language (write-string language output))
        (write-char #\Newline output)
        (write-string body output)
        (unless (and (plusp (length body))
                     (char= (char body (1- (length body))) #\Newline))
          (write-char #\Newline output))
        (write-string fence output)))))

(defun export-gfm-slug (title)
  (let ((slug-length
          (loop with output-length = 4
                with pending-separator = nil
                with wrote-character = nil
                for character across title
                do (if (alphanumericp character)
                       (progn
                         (when (and pending-separator wrote-character)
                           (incf output-length))
                         (incf output-length)
                         (setf pending-separator nil
                               wrote-character t))
                       (setf pending-separator t))
                finally (return (if wrote-character
                                    output-length
                                    (+ output-length 7))))))
    (export-gfm-check-total-output-length slug-length)
    (with-output-to-string (output)
      (write-string "org-" output)
      (loop with pending-separator = nil
            with wrote-character = nil
            for character across title
            do (if (alphanumericp character)
                   (progn
                     (when (and pending-separator wrote-character)
                       (write-char #\- output))
                     (setf pending-separator nil
                           wrote-character t)
                     (write-char (char-downcase character) output))
                   (setf pending-separator t))
            finally (unless wrote-character
                      (write-string "section" output))))))

(defun export-gfm-unique-id (state candidate)
  (export-gfm-check-total-output-length (length candidate))
  (let ((id candidate)
        (suffix 1))
    (loop while (gethash id (export-gfm-state-used-ids state))
          do (let* ((next-suffix (1+ suffix))
                    (suffix-text (write-to-string next-suffix)))
               (export-gfm-check-total-output-length
                (+ (length candidate) 1 (length suffix-text)))
               (setf suffix next-suffix
                     id (concatenate 'string candidate "-" suffix-text))))
    (setf (gethash id (export-gfm-state-used-ids state))
          t)
    id))

(defun export-gfm-target-id-safe-p (target-id)
  (and (stringp target-id)
       (notany #'export-control-character-p target-id)
       (notany #'export-whitespace-character-p target-id)
       (not (position (code-char 92) target-id))))

(defun export-gfm-register-reference (state name id)
  (when (and (stringp name) (plusp (length name)))
    (setf (gethash name (export-gfm-state-reference-ids state))
          id)))

(defun export-gfm-heading-reference (state node)
  (or (gethash node (export-gfm-state-heading-ids state))
      (let ((custom-id (node-property node :custom-id)))
        (export-gfm-unique-id
         state (if (and (stringp custom-id) (plusp (length custom-id))
                        (export-gfm-target-id-safe-p custom-id))
                   custom-id
                   (export-gfm-slug (or (node-property node :raw-value) "")))))))

(defun export-gfm-link-target-id (state node)
  (let* ((type (node-property node :type))
         (path (node-property node :path)))
    (when (and (stringp type) (stringp path))
      (cond
        ((string-equal type "custom-id")
         (or (gethash path (export-gfm-state-reference-ids state))
             path))
        ((member type '("fuzzy" "radio") :test #'string-equal)
         (gethash path (export-gfm-state-reference-ids state)))))))

(defun export-gfm-mark-heading-anchors (state)
  (let* ((preparation (export-gfm-state-preparation state))
         (views (export-collect-views
                 (export-preparation-root-views preparation))))
    (dolist (entry (export-preparation-toc preparation))
      (setf (gethash (export-view-node (getf entry :view))
                     (export-gfm-state-anchored-headings state))
            t))
    (dolist (view views)
      (let ((node (export-view-node view)))
        (when (eq (node-type node) :link)
          (let* ((target-id (export-gfm-link-target-id state node))
                 (heading (and target-id
                               (gethash target-id
                                        (export-gfm-state-heading-targets state)))))
            (when heading
              (setf (gethash heading
                             (export-gfm-state-anchored-headings state))
                    t))))))))

(defun export-gfm-index-state (state)
  (let* ((preparation (export-gfm-state-preparation state))
         (views (export-collect-views
                 (export-preparation-root-views preparation))))
    (dolist (view views)
      (let* ((node (export-view-node view))
             (type (node-type node)))
        (cond
          ((member type '(:headline :inlinetask) :test #'eq)
           (let* ((custom-id (node-property node :custom-id))
                  (title (node-property node :raw-value))
                  (candidate
                    (if (and (stringp custom-id) (plusp (length custom-id))
                             (export-gfm-target-id-safe-p custom-id))
                        custom-id
                        (export-gfm-slug (or title ""))))
                  (id (export-gfm-unique-id state candidate)))
             (setf (gethash node (export-gfm-state-heading-ids state)) id)
             (setf (gethash id (export-gfm-state-heading-targets state)) node)
             (export-gfm-register-reference state id id)
             (export-gfm-register-reference state custom-id id)
             (export-gfm-register-reference state title id)))
          ((member type '(:target :radio-target) :test #'eq)
           (let* ((value (node-property node :value))
                  (id (export-gfm-unique-id
                       state (export-gfm-slug (or value "")))))
             (export-gfm-register-reference state value id))))))
    (export-gfm-mark-heading-anchors state)
    state))

(defun export-gfm-join (strings separator)
  (let* ((nonempty (remove-if (lambda (string) (zerop (length string))) strings))
         (separator-length (* (max 0 (1- (length nonempty)))
                              (length separator)))
         (output-length (+ separator-length
                           (reduce #'+ nonempty :key #'length :initial-value 0))))
    (export-gfm-check-total-output-length output-length)
    (with-output-to-string (output)
      (loop with first-p = t
            for string in nonempty
            do (unless first-p (write-string separator output))
               (write-string string output)
               (setf first-p nil)))))

(defun export-gfm-view-trailing-post-blank (view)
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view)))
    (if (member type '(:org-data :section :headline :inlinetask :drawer
                       :special-block :center-block :macro)
                    :test #'eq)
        (let ((last-index
                (position-if-not
                 (lambda (child)
                   (eq (node-type (export-view-node child))
                       :footnote-definition))
                 children :from-end t)))
          (cond
            ((null last-index) 0)
            (t (export-gfm-view-trailing-post-blank
                (nth last-index children)))))
        (node-post-blank node))))

(defun export-gfm-strip-block-post-blank (view rendered)
  (export-node-strip-post-blank (export-view-node view) rendered))

(defun export-gfm-render-block-view
    (state view &key preserve-source-breaks-p)
  (let ((rendered (export-gfm-render-view
                   state view :mode :block
                   :preserve-source-breaks-p preserve-source-breaks-p)))
    (if (export-gfm-state-render-context state)
        (export-gfm-strip-block-post-blank view rendered)
        rendered)))

(defun export-gfm-render-views
    (state views &key preserve-source-breaks-p
                      (line-prefix-width 0) (newline-suffix-width 0))
  (with-output-to-string (output)
    (let ((previous-view nil)
          (first-p t)
          (output-length 0)
          (previous-newline-count 0))
      (dolist (view views)
        (let* ((separator-length
                 (if first-p 0
                     (+ 2 (export-gfm-view-trailing-post-blank previous-view))))
               (*export-gfm-output-prefix-length*
                 (+ *export-gfm-output-prefix-length*
                    output-length separator-length
                    (* line-prefix-width (1+ previous-newline-count))
                    (* newline-suffix-width previous-newline-count)))
               (rendered (export-gfm-render-block-view
                          state view
                          :preserve-source-breaks-p
                          preserve-source-breaks-p)))
          (unless (zerop (length rendered))
            (export-gfm-check-total-output-length
             (length rendered))
            (unless first-p
              (loop repeat separator-length do (write-char #\Newline output)))
            (write-string rendered output)
            (incf output-length (+ separator-length (length rendered)))
            (incf previous-newline-count
                  (+ separator-length (count #\Newline rendered)))
            (setf previous-view view
                  first-p nil)))))))

(defun export-gfm-render-inline-views
    (state views &key table-cell-p preserve-source-breaks-p)
  (with-output-to-string (output)
    (let ((output-length 0))
      (dolist (view views)
        (let* ((*export-gfm-output-prefix-length*
                 (+ *export-gfm-output-prefix-length* output-length))
               (rendered (export-gfm-render-inline-view
                          state view :table-cell-p table-cell-p
                          :preserve-source-breaks-p preserve-source-breaks-p)))
          (export-gfm-check-total-output-length (length rendered))
          (write-string rendered output)
          (incf output-length (length rendered)))))))

(defun export-gfm-inline-code-value-range (node)
  (let* ((property-value (node-property node :value))
         (document (node-document node))
         (source (and document (document-text document)))
         (node-start (node-begin node))
         (node-end (node-end node)))
    (multiple-value-bind (value start end)
        (cond
          ((stringp property-value)
           (values property-value 0 (length property-value)))
          ((and source node-start node-end)
           (values source node-start node-end))
          (t (values "" 0 0)))
      (let* ((type (node-type node))
             (delimiter (case type
                          (:code #\~)
                          (:verbatim #\=)))
             (trimmed-end (- end (node-post-blank node))))
        (if (and delimiter
                 (> (- trimmed-end start) 1)
                 (char= (char value start) delimiter)
                 (char= (char value (1- trimmed-end)) delimiter))
            (values value (1+ start) (1- trimmed-end))
            (values value start end))))))

(defun export-gfm-node-literal-part (node)
  (let* ((document (node-document node))
         (source (and document (document-text document)))
         (start (node-begin node))
         (end (node-end node)))
    (when (and source start end)
      (list (list source start end)))))

(defun export-gfm-literal-view-parts (view)
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view)))
    (cond
      ((member type '(:code :verbatim :inline-src-block) :test #'eq)
       (multiple-value-bind (text start end)
           (export-gfm-inline-code-value-range node)
         (list (list text start end))))
      ((eq type :entity)
       (let ((value (or (node-property node :utf-8)
                        (node-property node :unicode)
                        (node-property node :html))))
         (if (stringp value)
             (list (list value 0 (length value)))
             (or (export-gfm-node-literal-part node) nil))))
      ((eq type :link)
       (let ((value (or (node-property node :path) "")))
         (list (list value 0 (length value)))))
      (children
       (loop for child in children
             append (export-gfm-literal-view-parts child)))
      (t (or (export-gfm-node-literal-part node) nil)))))

(defun export-gfm-link-label (view &key table-cell-p)
  (let* ((node (export-view-node view))
         (children (export-view-children view))
         (parts (if children
                    (loop for child in children
                          append (export-gfm-literal-view-parts child))
                    (let ((text (or (node-property node :path) "")))
                      (list (list text 0 (length text)))))))
    (export-gfm-escape-literal-parts
     parts :table-cell-p table-cell-p
     :soft-newline-p (not table-cell-p))))

(defun export-gfm-serialized-destination-parts-output-length
    (parts &key table-cell-p)
  (+ 2
     (loop for part in parts
           sum (loop for character across part
                     sum (case character
                           ((#\Space #\< #\>) 3)
                           (#\| (if table-cell-p 3 1))
                           (#\& 5)
                           (t 1))))))

(defun export-gfm-serialize-destination-parts (parts &key table-cell-p)
  (export-gfm-check-total-output-length
   (export-gfm-serialized-destination-parts-output-length
    parts :table-cell-p table-cell-p))
  (with-output-to-string (output)
    (write-char #\< output)
    (loop for part in parts
          do (loop for character across part
                   do (case character
                        (#\Space (write-string "%20" output))
                        (#\< (write-string "%3C" output))
                        (#\> (write-string "%3E" output))
                        (#\| (if table-cell-p
                                 (write-string "%7C" output)
                                 (write-char character output)))
                        ;; Preserve literal URI ampersands after Markdown entity decoding.
                        (#\& (write-string "&amp;" output))
                        (t (write-char character output)))))
    (write-char #\> output)))

(defun export-gfm-node-source-range (node)
  (let* ((document (node-document node))
         (source (and document (document-text document)))
         (start (node-begin node))
         (end (node-end node)))
    (if (and (stringp source) start end
             (<= 0 start end (length source)))
        (values source start end)
        (let ((text (node-text node)))
          (values text 0 nil)))))

(defun export-gfm-link-source-control-p (node)
  (multiple-value-bind (source start end)
      (export-gfm-node-source-range node)
    (and source
         (loop for position from start below end
               always (not (export-control-character-p
                            (char source position)))))))

(defun export-gfm-destination-part-length (part)
  (if (and (consp part) (eq (first part) :range))
      (- (fourth part) (third part))
      (length part)))

(defun export-gfm-destination-part-character (part index)
  (if (and (consp part) (eq (first part) :range))
      (char (second part) (+ (third part) index))
      (char part index)))

(defun export-gfm-destination-parts-length (parts)
  (loop for part in parts sum (export-gfm-destination-part-length part)))

(defun export-gfm-destination-parts-character (parts index)
  (loop with remaining = index
        for part in parts
        for length = (export-gfm-destination-part-length part)
        when (< remaining length)
          do (return (export-gfm-destination-part-character part remaining))
        do (decf remaining length)))

(defun export-gfm-destination-parts-position (parts predicate)
  (loop with position = 0
        for part in parts
        do (loop for index below (export-gfm-destination-part-length part)
                 for character = (export-gfm-destination-part-character
                                  part index)
                 when (funcall predicate character)
                   do (return-from export-gfm-destination-parts-position
                        position)
                 do (incf position)))
  nil)

(defun export-gfm-destination-parts-explicit-scheme-end (parts)
  (loop with position = 0
        with valid-p = t
        for part in parts
        do (loop for index below (export-gfm-destination-part-length part)
                 for character = (export-gfm-destination-part-character
                                  part index)
                 do (progn
                      (cond
                        ((char= character #\:)
                         (return-from
                             export-gfm-destination-parts-explicit-scheme-end
                           (and (plusp position) valid-p position)))
                        ((zerop position)
                         (unless (export-ascii-alpha-p character)
                           (setf valid-p nil)))
                        ((not (export-ascii-scheme-character-p character))
                         (setf valid-p nil)))
                      (incf position)))))

(defun export-gfm-destination-parts-scheme-obfuscation-p (parts)
  (let ((entity (export-gfm-destination-parts-position
                 parts (lambda (character) (char= character #\&))))
        (boundary (export-gfm-destination-parts-position
                   parts (lambda (character) (find character "/?#")))))
    (and entity
         (null (export-gfm-destination-parts-explicit-scheme-end parts))
         (or (null boundary) (< entity boundary)))))

(defun export-gfm-destination-parts-prefix-equal-p (parts prefix)
  (and (>= (export-gfm-destination-parts-length parts) (length prefix))
       (loop for index below (length prefix)
             always (char= (export-gfm-destination-parts-character
                            parts index)
                           (char prefix index)))))

(defun export-gfm-destination-parts-allowed-scheme-p (parts end)
  (member-if (lambda (scheme)
               (and (= (length scheme) end)
                    (export-gfm-destination-parts-prefix-equal-p
                     parts scheme)))
             '("http" "https" "mailto")))

(defun export-gfm-destination-parts-safe-url-p (raw-parts destination-parts)
  (and (not (export-gfm-destination-parts-position
             raw-parts #'export-control-character-p))
       (not (export-gfm-destination-parts-position
             destination-parts #'export-control-character-p))
       (plusp (export-gfm-destination-parts-length destination-parts))
       (not (export-whitespace-character-p
             (export-gfm-destination-parts-character destination-parts 0)))
       (not (export-gfm-destination-parts-position
             destination-parts
             (lambda (character) (char= character #\\))))
       (not (export-gfm-destination-parts-prefix-equal-p
             destination-parts "//"))
       (let ((scheme-end
               (export-gfm-destination-parts-explicit-scheme-end
                destination-parts)))
         (or (null scheme-end)
             (export-gfm-destination-parts-allowed-scheme-p
              destination-parts scheme-end)))))

(defun export-gfm-link-destination-safe-p
    (type target-id raw-parts destination-parts)
  (and (if (and target-id (stringp type)
                (member type '("custom-id" "fuzzy" "radio")
                        :test #'string-equal))
           (export-gfm-target-id-safe-p target-id)
           t)
       (not (export-gfm-destination-parts-scheme-obfuscation-p raw-parts))
       (not (export-gfm-destination-parts-scheme-obfuscation-p
             destination-parts))
       (export-gfm-destination-parts-safe-url-p
        raw-parts destination-parts)))

(defun export-gfm-preflight-link-destination
    (type path target-id table-cell-p raw-parts destination-parts)
  (when (and raw-parts destination-parts
             (or (stringp path)
                 (and target-id (stringp type)
                      (member type '("custom-id" "fuzzy" "radio")
                              :test #'string-equal)))
             (export-gfm-link-destination-safe-p
              type target-id raw-parts destination-parts))
    (export-gfm-check-total-output-length
     (export-gfm-serialized-destination-parts-output-length
      destination-parts :table-cell-p table-cell-p))
    t))

(defun export-gfm-bracket-link-path-range (node)
  (multiple-value-bind (source start end)
      (export-gfm-node-source-range node)
    (when (and (stringp source)
               (>= (- end start) 4)
               (string= "[[" source :start2 start :end2 (+ start 2)))
      (let ((close (link-object-bracket-close source (+ start 2) end)))
        (when close
          (values source (+ start 2) close))))))

(defun export-gfm-raw-bracket-link-path (node)
  (multiple-value-bind (source start end)
      (export-gfm-bracket-link-path-range node)
    (when source
      (link-object-normalize-bracket-path
       (link-object-unescape-path (subseq source start end))))))

(defun export-gfm-expanded-bracket-path (node raw-path)
  (link-object-expand-abbreviation (node-document node) raw-path))

(defun export-gfm-link-abbreviation-prefix-unsafe-p (node type target-id)
  (multiple-value-bind (source start end)
      (export-gfm-bracket-link-path-range node)
    (when source
      (let* ((document (node-document node))
             (separator (position #\: source :start start :end end))
             (key-end (or separator end))
             (abbreviations
               (and document
                    (getf (document-settings document) :link-abbrevs)))
             (entry
               (find-if
                (lambda (candidate)
                  (let ((key (car candidate)))
                    (and (stringp key)
                         (= (length key) (- key-end start))
                         (string= key source :start2 start :end2 key-end))))
                abbreviations)))
        (when (and entry (stringp (cdr entry)))
          (let* ((replacement (cdr entry))
                 (placeholder (search "%s" replacement))
                 (destination-parts
                   (if separator
                       (if placeholder
                           (list (list :range replacement 0 placeholder)
                                 "x"
                                 (list :range replacement
                                       (+ placeholder 2)
                                       (length replacement)))
                           (list replacement "x"))
                       (if placeholder
                           (list (list :range replacement 0 placeholder)
                                 (list :range replacement
                                       (+ placeholder 2)
                                       (length replacement)))
                           (list replacement)))))
            (not (export-gfm-link-destination-safe-p
                  type target-id destination-parts destination-parts))))))))

(defun export-gfm-link-parsed-destination-unsafe-p (state node)
  (when (eq (node-property node :format) :bracket)
    (let* ((type (node-property node :type))
           (path (node-property node :path))
           (target-id (export-gfm-link-target-id state node))
           (internal-target-p
             (and target-id (stringp type)
                  (member type '("custom-id" "fuzzy" "radio")
                          :test #'string-equal)))
           (parts
             (and (stringp path)
                  (cond
                    (internal-target-p (list "#" target-id))
                    ((and (stringp type)
                          (member type *org-link-object-types*
                                  :test #'string=)
                          (not (and (string-equal type "file")
                                    (or (and (plusp (length path))
                                             (char= (char path 0) #\/))
                                        (and (>= (length path) 2)
                                             (string= "./" path :end2 2))
                                        (and (>= (length path) 3)
                                             (string= "../" path :end2 3))))))
                     (list type ":" path))
                    (t (list path))))))
      (or (and (not internal-target-p)
               (stringp type)
               (stringp path)
               (export-gfm-link-abbreviation-prefix-unsafe-p
                node type target-id))
          (and parts
               (not (export-gfm-link-destination-safe-p
                     type target-id parts parts)))))))

(defun export-gfm-source-range-matches-parts-p (source start end parts)
  (and (= (- end start) (export-gfm-destination-parts-length parts))
       (loop with position = start
             for part in parts
             always (loop for character across part
                          always (and (char= character (char source position))
                                      (progn (incf position) t)))
             finally (return (= position end)))))

(defun export-gfm-direct-bracket-link-destination
    (node type path table-cell-p)
  (when (and (eq (node-property node :format) :bracket)
             (stringp type)
             (stringp path)
             (member type '("http" "https" "mailto")
                     :test #'string-equal))
    (let ((parts (list type ":" path)))
      (multiple-value-bind (source start end)
          (export-gfm-bracket-link-path-range node)
        (when (and source
                   (export-gfm-source-range-matches-parts-p
                    source start end parts))
          (values
           t
           (export-gfm-serialize-destination-parts
            parts :table-cell-p table-cell-p)))))))

(defun export-gfm-link-destination-candidates (state node)
  (let* ((type (node-property node :type))
         (path (node-property node :path))
         (format (node-property node :format))
         (explicit-url-p
           (and (stringp type) (stringp path)
                (member type '("http" "https" "mailto")
                        :test #'string-equal)))
         (target-id (export-gfm-link-target-id state node))
         (internal-target-p
           (and target-id (stringp type)
                (member type '("custom-id" "fuzzy" "radio")
                        :test #'string-equal)))
         (raw-path (and (eq format :bracket) (not internal-target-p)
                        (export-gfm-raw-bracket-link-path node)))
         (raw-destination (or raw-path (if internal-target-p target-id path)))
         (raw-parts
           (cond
             (internal-target-p (list "#" target-id))
             (raw-path (list raw-path))
             (explicit-url-p (list type ":" path))
             (raw-destination (list raw-destination))))
         (destination-parts
           (cond
             (internal-target-p (list "#" target-id))
             ((eq format :bracket)
              (let ((expanded-path
                      (and raw-path
                           (export-gfm-expanded-bracket-path node raw-path))))
                (and expanded-path (list expanded-path))))
             (explicit-url-p (list type ":" path))
             ((and (stringp type) (stringp path)) nil)
             ((stringp path) (list path)))))
    (values type path target-id raw-parts destination-parts)))

(defun export-gfm-link-destination (state node &key table-cell-p)
  (when (and (export-gfm-link-source-control-p node)
             (not (export-gfm-link-parsed-destination-unsafe-p state node)))
    (multiple-value-bind (type path)
        (values (node-property node :type)
                (node-property node :path))
      (multiple-value-bind (handled-p destination)
          (export-gfm-direct-bracket-link-destination
           node type path table-cell-p)
        (if handled-p
            destination
            (multiple-value-bind (type path target-id raw-parts
                                  destination-parts)
                (export-gfm-link-destination-candidates state node)
              (when (and destination-parts
                         (export-gfm-preflight-link-destination
                          type path target-id table-cell-p
                          raw-parts destination-parts))
                (export-gfm-serialize-destination-parts
                 destination-parts :table-cell-p table-cell-p))))))))

(defun export-gfm-image-path-p (path) (export-image-path-p path))

(defun export-gfm-render-link (state view &key table-cell-p)
  (let* ((node (export-view-node view))
         (destination (export-gfm-link-destination
                       state node :table-cell-p table-cell-p))
         (image-p (and destination
                        (export-gfm-image-path-p
                         (node-property node :path))))
         (label-reserve (if destination
                            (+ (length destination) 4 (if image-p 1 0))
                            0))
         (label (export-gfm-render-with-prefix
                 label-reserve
                 (lambda ()
                   (export-gfm-link-label view :table-cell-p table-cell-p)))))
    (if destination
        (progn
          (export-gfm-check-total-output-length
           (+ label-reserve (length label)))
          (format nil "~A[~A](~A)" (if image-p "!" "") label destination))
        label)))

(defun export-gfm-render-target (state view)
  (let* ((node (export-view-node view))
         (value (node-property node :value))
         (id (or (gethash (and (stringp value) value)
                          (export-gfm-state-reference-ids state))
                 (export-gfm-slug (or value ""))))
         (attribute-length (export-gfm-html-attribute-output-length id))
         (radio-p (eq (node-type node) :radio-target))
         (text-length (if radio-p
                          (export-gfm-escaped-literal-output-length
                           (or value ""))
                          0)))
    (export-gfm-check-total-output-length
     (+ (length "<a id=\"") attribute-length
        (if radio-p
            (+ (length "\">") text-length (length "</a>"))
            (length "\"></a>"))))
    (if radio-p
        (format nil "<a id=\"~A\">~A</a>"
                (export-gfm-escape-html-attribute id)
                (export-gfm-escape-literal (or value "")))
        (format nil "<a id=\"~A\"></a>"
                (export-gfm-escape-html-attribute id)))))

(defun export-gfm-render-inline-view-legacy
    (state view &key table-cell-p preserve-source-breaks-p
                 (include-post-blank-p t))
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view))
         (content-reserve
           (case type
             (:bold 4)
             (:italic 2)
             (:strike-through 4)
             (:underline 7)
             ((:subscript :superscript) 11)
             (otherwise 0)))
         (content (lambda ()
                    (export-gfm-render-with-prefix
                     content-reserve
                     (lambda ()
                       (export-gfm-render-inline-views
                        state children :table-cell-p table-cell-p
                        :preserve-source-breaks-p
                        preserve-source-breaks-p)))))
         (rendered
           (case type
             ((:plain-text :text)
              (export-gfm-escape-node-source-text
               node :table-cell-p table-cell-p
               :soft-newline-p
               (not (or table-cell-p preserve-source-breaks-p))
               :trim-terminal-newline-p preserve-source-breaks-p))
             (:bold (format nil "**~A**" (funcall content)))
             (:italic (format nil "*~A*" (funcall content)))
             (:strike-through
              (concatenate 'string "~~" (funcall content) "~~"))
             (:underline (format nil "<u>~A</u>" (funcall content)))
             (:subscript (format nil "<sub>~A</sub>" (funcall content)))
             (:superscript (format nil "<sup>~A</sup>" (funcall content)))
             ((:code :verbatim :inline-src-block)
              (multiple-value-bind (value start end)
                  (export-gfm-inline-code-value-range node)
                (export-gfm-code-span value :start start :end end
                                      :table-cell-p table-cell-p)))
             (:entity
              (export-gfm-escape-literal
               (or (node-property node :utf-8) (node-property node :unicode)
                   (node-property node :html) (node-text node) "")
               :table-cell-p table-cell-p))
             (:link (export-gfm-render-link state view :table-cell-p table-cell-p))
             (:export-snippet (export-gfm-render-snippet state node))
             (:line-break (if table-cell-p "<br>" (format nil "  ~%")))
             (:footnote-reference
              (export-gfm-render-footnote-reference state view))
             (:table-cell
              (if children
                  (export-gfm-render-inline-views
                   state children :table-cell-p t
                   :preserve-source-breaks-p preserve-source-breaks-p)
                  (export-gfm-escape-literal
                   (or (node-property node :value) (node-text node) "")
                   :table-cell-p t)))
             ((:target :radio-target)
              (export-gfm-render-target state view))
             ((:babel-call :inline-babel-call :citation :citation-reference
               :latex-fragment :statistics-cookie :timestamp)
              (export-gfm-escape-node-source-text
               node :table-cell-p table-cell-p
               :soft-newline-p
               (not (or table-cell-p preserve-source-breaks-p))))
             (:macro (funcall content))
             (otherwise
              (if children
                  (funcall content)
                  (export-gfm-escape-node-source-text
                   node :table-cell-p table-cell-p
                   :soft-newline-p
                   (not (or table-cell-p preserve-source-breaks-p))))))))
    (if (and include-post-blank-p (plusp (node-post-blank node)))
        (let ((output-length (+ (length rendered) (node-post-blank node))))
          (export-gfm-check-total-output-length output-length)
          (concatenate 'string rendered
                       (make-string (node-post-blank node)
                                    :initial-element #\Space)))
        rendered)))

(defun export-gfm-render-inline-view
    (state view &key table-cell-p preserve-source-breaks-p)
  (if (export-gfm-state-render-context state)
      (export-gfm-dispatch-view
       state view :mode :inline :table-cell-p table-cell-p
       :preserve-source-breaks-p preserve-source-breaks-p)
      (export-gfm-render-inline-view-legacy
       state view :table-cell-p table-cell-p
       :preserve-source-breaks-p preserve-source-breaks-p)))

(defun export-gfm-render-heading (state view)
  (let* ((node (export-view-node view))
         (level (or (node-property node :level) 1))
         (title (or (node-property node :raw-value) ""))
         (tags (node-property node :tags))
         (id (export-gfm-heading-reference state node))
         (anchored-p (gethash node
                              (export-gfm-state-anchored-headings state)))
         (title-output-length
           (export-gfm-escaped-literal-output-length title))
         (tag-output-length
           (if tags
               (+ 7
                  (reduce #'+ tags :key #'length :initial-value 0)
                  (max 0 (1- (length tags))))
               0))
         (heading-output-length
           (+ (if anchored-p
                  (+ 15 (export-gfm-html-attribute-output-length id))
                  0)
              (min 6 (max 1 level)) 1 title-output-length
              tag-output-length))
         (heading
           (progn
             (export-gfm-check-total-output-length heading-output-length)
             (with-output-to-string (output)
               (when anchored-p
                 (format output "<a id=\"~A\"></a>~%~%"
                         (export-gfm-escape-html-attribute id)))
               (write-string (make-string (min 6 (max 1 level))
                                          :initial-element #\#)
                             output)
               (write-char #\Space output)
               (write-string (export-gfm-escape-literal title) output)
               (when tags
                 (write-string "     :" output)
                 (write-string (format nil "~{~A~^:~}" tags) output)
                 (write-char #\: output)))))
         (children
           (export-gfm-render-with-prefix
            (+ (length heading) 2)
            (lambda ()
              (export-gfm-render-views state (export-view-children view))))))
    (if (zerop (length children))
        heading
        (progn
          (export-gfm-check-total-output-length
           (+ (length heading) 2 (length children)))
          (format nil "~A~%~%~A" heading children)))))

(defun export-gfm-toc (state)
  (let ((entries (export-preparation-toc
                  (export-gfm-state-preparation state))))
    (with-output-to-string (output)
      (let ((output-length 0)
            (first-p t))
        (dolist (entry entries)
          (let* ((separator-length (if first-p 0 1))
                 (*export-gfm-output-prefix-length*
                   (+ *export-gfm-output-prefix-length*
                      output-length separator-length))
                 (view (getf entry :view))
                 (node (export-view-node view))
                 (level (getf entry :level))
                 (indent (make-string (* 2 (1- level)) :initial-element #\Space))
                 (id (export-gfm-heading-reference state node))
                 (destination
                   (export-gfm-serialize-destination-parts (list "#" id)))
                 (title
                   (export-gfm-render-with-prefix
                    (+ (length indent) 6 (length destination))
                    (lambda ()
                      (export-gfm-escape-literal
                       (or (getf entry :title) "")))))
                 (line (format nil "~A- [~A](~A)" indent title destination)))
            (export-gfm-check-total-output-length
             (length line))
            (unless first-p (write-char #\Newline output))
            (write-string line output)
            (incf output-length (+ separator-length (length line)))
            (setf first-p nil)))))))

(defun export-gfm-render-quote (state view)
  (let ((text (export-gfm-render-views
               state (export-view-children view) :line-prefix-width 2)))
    (export-gfm-check-total-output-length
     (+ (length text) (* 2 (1+ (count #\Newline text)))))
    (with-output-to-string (output)
      (loop with start = 0
            for newline = (position #\Newline text :start start)
            do (write-string "> " output)
               (write-string text output :start start :end newline)
               (when newline
                 (write-char #\Newline output)
                 (setf start (1+ newline)))
            while newline))))

(defun export-gfm-render-list-item (state view marker depth)
  (let* ((marker (or *export-gfm-list-marker* marker))
         (depth (if *export-gfm-list-marker* *export-gfm-list-depth* depth))
         (children (export-view-children view))
         (paragraph (find-if
                     (lambda (child)
                       (eq (node-type (export-view-node child)) :paragraph))
                     children))
         (prefix (format nil "~A~A "
                         (make-string (* 2 depth) :initial-element #\Space)
                         marker))
         (checkbox (node-property (export-view-node view) :checkbox))
         (checkbox-text (case checkbox
                          (:off "[ ] ")
                          (:on "[X] ")
                          (:trans "[-] ")))
         (first-text
           (export-gfm-render-with-prefix
            (+ (length prefix) (length (or checkbox-text "")))
            (lambda ()
              (if paragraph
                  (export-gfm-render-block-view state paragraph)
                  (if children
                      (export-gfm-render-block-view state (first children))
                      "")))))
         (remaining (if paragraph (remove paragraph children :test #'eq)
                        (rest children))))
    (with-output-to-string (output)
      (let ((output-length (+ (length prefix) (length (or checkbox-text ""))
                              (length first-text))))
        (export-gfm-check-total-output-length output-length)
        (write-string prefix output)
        (when checkbox-text (write-string checkbox-text output))
        (write-string first-text output)
        (dolist (child remaining)
          (let* ((continuation-indent (* 2 (1+ depth)))
                 (outer-prefix *export-gfm-output-prefix-length*)
                 (*export-gfm-output-prefix-length*
                   (+ outer-prefix output-length 1))
                 (rendered
                   (export-gfm-render-with-prefix
                    continuation-indent
                    (lambda ()
                      (export-gfm-render-block-view state child)))))
            (unless (zerop (length rendered))
              (let ((line-indent-count
                      (* (1+ (count #\Newline rendered)) continuation-indent)))
                (let ((*export-gfm-output-prefix-length* outer-prefix))
                  (export-gfm-check-total-output-length
                   (+ output-length 1 (length rendered) line-indent-count))))
              (write-char #\Newline output)
              (loop with start = 0
                    for newline = (position #\Newline rendered :start start)
                    do (write-string
                        (make-string continuation-indent :initial-element #\Space)
                        output)
                       (write-string rendered output :start start :end newline)
                       (when newline
                         (write-char #\Newline output)
                         (setf start (1+ newline)))
                    while newline)
              (incf output-length
                    (+ 1 (length rendered)
                       (* (1+ (count #\Newline rendered))
                          continuation-indent))))))))))

(defun export-gfm-render-list (state view)
  (let* ((node (export-view-node view))
         (kind (node-property node :type))
         (ordered-p (eq kind :ordered)))
    (with-output-to-string (output)
      (let ((output-length 0)
            (first-p t))
        (loop for item in (export-view-children view)
              for number from 1
              for item-node = (export-view-node item)
              for marker = (if ordered-p
                               (format nil "~A."
                                       (or (node-property item-node :counter)
                                           number))
                               (or (node-property item-node :bullet) "-"))
              for separator-length = (if first-p 0 1)
              for rendered =
                (let ((*export-gfm-output-prefix-length*
                        (+ *export-gfm-output-prefix-length*
                           output-length separator-length))
                      (*export-gfm-list-marker* marker)
                      (*export-gfm-list-depth* 0))
                  (export-gfm-render-block-view state item))
              unless (zerop (length rendered))
                do (export-gfm-check-total-output-length
 (+ output-length separator-length (length rendered)))
                   (unless first-p (write-char #\Newline output))
                   (write-string rendered output)
                   (incf output-length (+ separator-length (length rendered)))
                   (setf first-p nil))))))

(defun export-gfm-table-cell (state view)
  (if (export-gfm-state-render-context state)
      (export-gfm-strip-block-post-blank
       view
       (export-gfm-render-view
        state view :mode :inline :table-cell-p t))
      (let ((children (export-view-children view)))
        (if children
            (export-gfm-render-inline-views state children :table-cell-p t)
            (export-gfm-escape-literal
             (or (node-property (export-view-node view) :value)
                 (node-text (export-view-node view)) "")
             :table-cell-p t)))))

(defun export-gfm-table-row (state view column-count)
  (let ((cells (export-view-children view)))
    (with-output-to-string (output)
      (write-char #\| output)
      (let ((output-length 1))
        (loop for index below column-count
              for cell = (nth index cells)
              for rendered =
                (when cell
                  (let ((*export-gfm-output-prefix-length*
                          (+ *export-gfm-output-prefix-length*
                             output-length 1)))
                    (export-gfm-table-cell state cell)))
              do (when cell
                   (export-gfm-check-total-output-length
                    (+ 3 (length rendered))))
                 (write-char #\Space output)
                 (when cell (write-string rendered output))
                 (write-string " |" output)
                 (incf output-length (+ 3 (length (or rendered ""))))
                 (export-gfm-check-total-output-length output-length))))))

(defun export-gfm-render-table-row-for-parent (state view column-count)
  (let ((*export-gfm-table-column-count* column-count))
    (export-gfm-render-block-view state view)))

(defun export-gfm-render-table (state view)
  (let* ((rows (remove-if
                (lambda (row)
                  (eq (node-property (export-view-node row) :type) :rule))
                (export-view-children view)))
         (header (first rows))
         (body (rest rows))
         (column-count (length (and header (export-view-children header)))))
    (if (zerop column-count)
        ""
        (with-output-to-string (output)
          (let* ((outer-prefix *export-gfm-output-prefix-length*)
                 (header-row (export-gfm-render-with-prefix
                              0 (lambda ()
                                  (export-gfm-render-table-row-for-parent
                                   state header column-count))))
                 (output-length (length header-row))
                 (separator-length (+ 2 (* 6 column-count))))
            (export-gfm-check-total-output-length
             (+ output-length separator-length))
            (write-string header-row output)
            (write-char #\Newline output)
            (write-char #\| output)
            (loop repeat column-count do (write-string " --- |" output))
            (incf output-length separator-length)
            (dolist (row body)
              (let* ((*export-gfm-output-prefix-length*
                       (+ outer-prefix output-length 1))
                     (row-text
                       (export-gfm-render-table-row-for-parent
                        state row column-count)))
                (export-gfm-check-total-output-length (length row-text))
                (write-char #\Newline output)
                (write-string row-text output)
                (incf output-length (1+ (length row-text))))))))))

(defun export-gfm-render-footnote-reference (state view)
  (let* ((node (export-view-node view))
         (label (node-property node :label))
         (entry (and label
                     (find label (export-preparation-footnotes
                                  (export-gfm-state-preparation state))
                           :key (lambda (item) (getf item :label))
                           :test #'equal))))
    (cond
      (entry
        (let* ((number (getf entry :number))
               (count (1+ (gethash label
                                   (export-gfm-state-footnote-reference-counts state)
                                   0)))
               (suffix (if (= count 1) ""
                           (format nil ".~D" (1- count))))
               (label-length (export-gfm-html-attribute-output-length label))
               (reference-id-length (+ 4 label-length (length suffix)))
               (number-text (princ-to-string number))
               (reference-id nil))
          (export-gfm-check-total-output-length
           (+ (length "<sup><a id=\"") reference-id-length
              (length "\" class=\"footref\" href=\"#fn.") label-length
              (length "\" role=\"doc-backlink\">")
              (length number-text) (length "</a></sup>")))
          (setf (gethash label
                         (export-gfm-state-footnote-reference-counts state))
                count
                reference-id (concatenate 'string "fnr." label suffix))
          (format nil
                  "<sup><a id=\"~A\" class=\"footref\" href=\"#fn.~A\" role=\"doc-backlink\">~D</a></sup>"
                  (export-gfm-escape-html-attribute reference-id)
                  (export-gfm-escape-html-attribute label)
                  number)))
      ((eq (node-property node :type) :inline)
       (export-gfm-render-inline-views state (export-view-children view)))
      (t (export-gfm-escape-literal (or label ""))))))

(progn
(defun export-gfm-render-footnote-definition-body (state view)
  (let ((children (export-view-children view)))
    (with-output-to-string (output)
      (let ((outer-prefix *export-gfm-output-prefix-length*)
            (output-length 0)
            (first-p t))
        (dolist (child children)
          (let* ((separator-length (if first-p 0 2))
                 (*export-gfm-output-prefix-length*
                   (+ outer-prefix output-length separator-length))
                 (rendered (export-gfm-render-block-view state child)))
            (unless (zerop (length rendered))
              (export-gfm-check-total-output-length (length rendered))
              (unless first-p (write-string (format nil "~%~%") output))
              (write-string rendered output)
              (incf output-length (+ separator-length (length rendered)))
              (setf first-p nil)))))))))

(progn
(defun export-gfm-render-footnote-definition (state entry)
  (let* ((number (getf entry :number))
         (view (getf entry :definition))
         (header (format nil
                         "<sup><a id=\"fn.~A\" class=\"footnum\" href=\"#fnr.~A\">~D</a></sup> "
                         number number number))
         (body
           (let ((*export-gfm-output-prefix-length*
                   (+ *export-gfm-output-prefix-length* (length header)))
                 (*export-gfm-render-footnote-definition-body-p* t))
             (export-gfm-render-block-view state view))))
    (export-gfm-check-total-output-length (+ (length header) (length body)))
    (concatenate 'string header body)))

(defun export-gfm-render-footnotes (state)
  (let ((entries (export-preparation-footnotes
                  (export-gfm-state-preparation state))))
    (when entries
      (let ((heading "## Footnotes"))
        (export-gfm-check-total-output-length (+ (length heading) 2))
        (with-output-to-string (output)
          (write-string heading output)
          (write-string (format nil "~%~%") output)
          (let ((outer-prefix *export-gfm-output-prefix-length*)
                (output-length (+ (length heading) 2))
                (first-p t))
            (dolist (entry entries)
              (let* ((separator-length (if first-p 0 2))
                     (*export-gfm-output-prefix-length*
                       (+ outer-prefix output-length separator-length))
                     (rendered
                       (export-gfm-render-footnote-definition state entry)))
                (export-gfm-check-total-output-length (length rendered))
                (unless first-p (write-string (format nil "~%~%") output))
                (write-string rendered output)
                (incf output-length (+ separator-length (length rendered)))
                (setf first-p nil))))))))))

(defun export-gfm-render-src-block (state view)
  (let* ((node (export-view-node view))
         (mode (getf (export-view-payload view) :exports))
         (value (or (node-property node :value) ""))
         (code (export-gfm-fenced-block value
                                        (export-gfm-source-language node)))
         (results-prefix
           (if (member mode '(:results :code :none) :test #'eq)
               *export-gfm-output-prefix-length*
               (+ *export-gfm-output-prefix-length* (length code) 2)))
         (results
           (let ((*export-gfm-output-prefix-length* results-prefix))
             (export-gfm-render-views state (export-view-children view)))))
    (case mode
      (:none "")
      (:results results)
      (:code code)
      (otherwise (if (zerop (length results)) code
                     (progn
                       (export-gfm-check-total-output-length
                        (+ (length code) 2 (length results)))
                       (format nil "~A~%~%~A" code results)))))))

(defun export-gfm-raw-html-enabled-p (state)
  (let ((context (export-gfm-state-render-context state)))
    (if context
        (export-render-context-raw-output-p context :html)
        (export-preparation-allow-raw-html
         (export-gfm-state-preparation state)))))

(defun export-gfm-render-export-block (state node)
  (let ((backend (node-property node :type))
        (value (or (node-property node :value) "")))
    (if (and (stringp backend) (string-equal backend "gfm"))
        (if (export-gfm-raw-html-enabled-p state)
            value
            (export-gfm-fenced-block value))
        "")))

(defun export-gfm-render-snippet (state node)
  (let ((backend (node-property node :back-end))
        (value (or (node-property node :value) "")))
    (if (and (stringp backend) (string-equal backend "gfm"))
        (if (export-gfm-raw-html-enabled-p state)
            value
            (export-gfm-escape-literal value))
        "")))

(defun export-gfm-render-view-legacy
    (state view &key preserve-source-breaks-p)
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view)))
    (case type
      ((:org-data :section :drawer :special-block :center-block :macro)
       (export-gfm-render-views
        state children :preserve-source-breaks-p preserve-source-breaks-p))
      ((:headline :inlinetask)
       (export-gfm-render-heading state view))
      (:paragraph
       (export-gfm-render-inline-views
        state children :preserve-source-breaks-p preserve-source-breaks-p))
      (:quote-block (export-gfm-render-quote state view))
      (:plain-list (export-gfm-render-list state view))
      (:item (export-gfm-render-list-item state view "-" 0))
      (:table (export-gfm-render-table state view))
      (:table-row
       (if *export-gfm-table-column-count*
           (export-gfm-table-row state view *export-gfm-table-column-count*)
           (export-gfm-render-inline-views state children :table-cell-p t)))
      (:table-cell
       (if children
           (export-gfm-render-inline-views state children :table-cell-p t)
           (export-gfm-escape-literal
            (or (node-property node :value) (node-text node) "")
            :table-cell-p t)))
      ((:example-block :fixed-width)
       (export-gfm-fenced-block (or (node-property node :value) "")))
      (:src-block (export-gfm-render-src-block state view))
      (:dynamic-block
       (export-gfm-escape-literal
        (or (node-property node :value) (node-text node) "")))
      (:export-block (export-gfm-render-export-block state node))
      (:horizontal-rule "---")
      (:verse-block
       (let ((content (export-gfm-render-views
 state children :preserve-source-breaks-p t :newline-suffix-width 2)))
         (export-gfm-check-total-output-length
          (+ (length content) (* 2 (count #\Newline content))))
         (with-output-to-string (output)
           (loop with start = 0
                 for newline = (position #\Newline content :start start)
                 do (write-string content output :start start :end newline)
                    (when newline
                      (write-string "  " output)
                      (write-char #\Newline output)
                      (setf start (1+ newline)))
                 while newline))))
      (:footnote-definition
 (if *export-gfm-render-footnote-definition-body-p*
     (export-gfm-render-footnote-definition-body state view)
     ""))
      ((:comment :comment-block :keyword :property-drawer :node-property
        :planning :clock)
       "")
      ((:bold :italic :strike-through :underline :subscript :superscript
        :code :verbatim :inline-src-block :entity :link :export-snippet
        :line-break :footnote-reference :target :radio-target :babel-call
        :inline-babel-call :citation :citation-reference :latex-fragment
        :statistics-cookie :timestamp :plain-text :text)
       (export-gfm-render-inline-view-legacy
        state view :preserve-source-breaks-p preserve-source-breaks-p))
      (:latex-environment
       (export-gfm-escape-node-source-text node))
      (otherwise (export-gfm-escape-node-source-text node)))))

(defun export-gfm-render-view
    (state view &key (mode :block) table-cell-p preserve-source-breaks-p)
  (if (export-gfm-state-render-context state)
      (export-gfm-dispatch-view
       state view :mode mode :table-cell-p table-cell-p
       :preserve-source-breaks-p preserve-source-breaks-p)
      (export-gfm-render-view-legacy
       state view :preserve-source-breaks-p preserve-source-breaks-p)))

(defun export-gfm-dispatch-view
    (state view &key (mode :block) table-cell-p preserve-source-breaks-p)
  (let ((context (export-gfm-state-render-context state)))
    (if context
        (let ((*export-current-output-offset*
                *export-gfm-output-prefix-length*))
          (export-render-view
           context view :mode mode :table-cell-p table-cell-p
           :preserve-source-breaks-p preserve-source-breaks-p))
        (if (eq mode :inline)
            (export-gfm-render-inline-view-legacy
             state view :table-cell-p table-cell-p
             :preserve-source-breaks-p preserve-source-breaks-p)
            (export-gfm-render-view-legacy
             state view :preserve-source-breaks-p
             preserve-source-breaks-p)))))

(defun export-gfm-render-node-legacy (state node view frame)
  (if view
      (if (eq (export-render-frame-mode frame) :inline)
          (export-gfm-render-inline-view-legacy
           state view
           :table-cell-p (export-render-frame-table-cell-p frame)
           :preserve-source-breaks-p
           (export-render-frame-preserve-source-breaks-p frame)
           :include-post-blank-p nil)
          (export-gfm-render-view-legacy
           state view :preserve-source-breaks-p
           (export-render-frame-preserve-source-breaks-p frame)))
      (export-gfm-escape-node-source-text node)))

(defun export-gfm-transcode-node (node view frame context)
  (let* ((state (export-render-context-state context))
         (parent-context (export-gfm-state-render-context state))
         (parent-frame (export-gfm-state-render-frame state))
         (parent-offset (export-gfm-state-output-offset state))
         (offset (max parent-offset
                      (export-render-frame-output-offset frame))))
    (setf (export-gfm-state-preparation state)
          (org-kit::%export-render-context-preparation context)
          (export-gfm-state-limits state) (export-render-context-limits context))
    (unless (export-gfm-state-indexed-p state)
      (let ((*export-gfm-output-limits* (export-render-context-limits context))
            (*export-gfm-output-prefix-length* parent-offset))
        (export-gfm-index-state state)
        (setf (export-gfm-state-indexed-p state) t)))
    (setf (export-gfm-state-render-context state) context
          (export-gfm-state-render-frame state) frame
          (export-gfm-state-output-offset state) offset)
    (unwind-protect
         (let ((*export-gfm-output-limits* (export-render-context-limits context))
               (*export-gfm-output-prefix-length* offset)
               (*export-current-output-offset* offset))
           (export-gfm-render-node-legacy state node view frame))
      (setf (export-gfm-state-render-context state) parent-context
            (export-gfm-state-render-frame state) parent-frame
            (export-gfm-state-output-offset state) parent-offset))))

(defun export-gfm-node-transcoders ()
  (mapcar (lambda (type)
            (cons type #'export-gfm-transcode-node))
          (node-types)))

(defun export-gfm-node-policies ()
  '((:diary-sexp :unsupported
                 :feature :diary-sexp
                 :message "Diary sexp export is unsupported")))

(defun export-gfm-render-body (context)
  (let* ((state (export-render-context-state context))
         (preparation (org-kit::%export-render-context-preparation context))
         (base-offset *export-current-output-offset*)
         (parent-context (export-gfm-state-render-context state))
         (parent-frame (export-gfm-state-render-frame state))
         (parent-offset (export-gfm-state-output-offset state)))
    (setf (export-gfm-state-preparation state) preparation
          (export-gfm-state-limits state) (export-render-context-limits context)
          (export-gfm-state-render-context state) context
          (export-gfm-state-render-frame state) nil
          (export-gfm-state-output-offset state) base-offset)
    (unwind-protect
         (let ((*export-gfm-output-limits* (export-render-context-limits context))
               (*export-gfm-output-prefix-length* base-offset)
               (*export-current-output-offset* base-offset))
           (unless (export-gfm-state-indexed-p state)
             (export-gfm-index-state state)
             (setf (export-gfm-state-indexed-p state) t))
           (export-gfm-render-document state))
      (setf (export-gfm-state-render-context state) parent-context
            (export-gfm-state-render-frame state) parent-frame
            (export-gfm-state-output-offset state) parent-offset))))

(defun export-gfm-render-document (state)
  (let* ((preparation (export-gfm-state-preparation state))
         (outer-prefix *export-gfm-output-prefix-length*)
         (toc (export-gfm-toc state))
         (body-prefix (+ outer-prefix (length toc)
                         (if (plusp (length toc)) 2 0)))
         (body
           (let ((*export-gfm-output-prefix-length* body-prefix))
             (export-gfm-render-views
              state (export-preparation-root-views preparation))))
         (footnotes-prefix
           (+ outer-prefix (length toc) (length body)
              (if (plusp (length toc)) 2 0)
              (if (plusp (length body)) 2 0)))
         (footnotes
           (let ((*export-gfm-output-prefix-length* footnotes-prefix))
             (export-gfm-render-footnotes state))))
    (let ((*export-gfm-output-prefix-length* outer-prefix))
      (export-gfm-join
       (remove-if-not (lambda (text)
                        (and (stringp text) (plusp (length text))))
                      (list toc body footnotes))
       (format nil "~%~%")))))

(defmethod export-render-preparation
    ((preparation export-preparation) (backend (eql :gfm)))
  (let* ((state (export-gfm-state-for-preparation preparation))
         (*export-gfm-output-limits* (export-gfm-state-limits state))
         (*export-gfm-output-prefix-length* 0))
    (progn
(export-gfm-index-state state)
(setf (export-gfm-state-indexed-p state) t))
    (export-gfm-render-document state)))

(eval-when (:load-toplevel :execute)
  (%register-builtin-export-backend
   :gfm
   :renderer #'export-gfm-render-body
   :make-state #'export-gfm-make-state
   :transcoders (export-gfm-node-transcoders)
   :node-policies (export-gfm-node-policies)
   :raw-output-options '((:html . :allow-raw-html))
   :export-block-names '("gfm")
   :export-snippet-names '("gfm")))
