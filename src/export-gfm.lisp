;;;; src/export-gfm.lisp
(in-package #:org-kit)

(defstruct (export-gfm-state (:constructor %make-export-gfm-state))
  preparation
  (heading-ids (make-hash-table :test #'eq))
  (heading-targets (make-hash-table :test #'equal))
  (anchored-headings (make-hash-table :test #'eq))
  (reference-ids (make-hash-table :test #'equal))
  (used-ids (make-hash-table :test #'equal))
  (footnote-reference-counts (make-hash-table :test #'equal)))

(defun export-gfm-ascii-punctuation-p (character)
  (let ((code (char-code character)))
    (or (<= #x21 code #x2f)
        (<= #x3a code #x40)
        (<= #x5b code #x60)
        (<= #x7b code #x7e))))

(defun export-gfm-normalize-newlines (text)
  (with-output-to-string (output)
    (loop with index = 0
          while (< index (length text))
          for character = (char text index)
          do (cond
               ((char= character #\Return)
                (write-char #\Newline output)
                (when (and (< (1+ index) (length text))
                           (char= (char text (1+ index)) #\Newline))
                  (incf index)))
               (t (write-char character output)))
             (incf index))))

(defun export-gfm-escape-literal
    (text &key table-cell-p soft-newline-p trim-terminal-newline-p)
  (with-output-to-string (output)
    (loop with index = 0
          while (< index (length text))
          for character = (char text index)
          do (cond
               ((char= character #\Tab) (write-char #\Space output))
               ((member character '(#\Return #\Newline) :test #'char=)
                (cond
                  (table-cell-p
                   (write-string "<br>" output)
                   (when (and (char= character #\Return)
                              (< (1+ index) (length text))
                              (char= (char text (1+ index)) #\Newline))
                     (incf index)))
                  ((and trim-terminal-newline-p
                        (loop for position from index below (length text)
                              always (member (char text position)
                                             '(#\Return #\Newline)
                                             :test #'char=)))
                   (setf index (length text)))
                  (soft-newline-p
                   (loop while (and (< index (length text))
                                    (member (char text index)
                                            '(#\Return #\Newline)
                                            :test #'char=))
                         do (when (and (char= (char text index) #\Return)
                                       (< (1+ index) (length text))
                                       (char= (char text (1+ index)) #\Newline))
                              (incf index))
                            (incf index))
                   (when (< index (length text))
                     (write-char #\Space output))
                   (decf index))
                  (t
                   (write-char #\Newline output)
                   (when (and (char= character #\Return)
                              (< (1+ index) (length text))
                              (char= (char text (1+ index)) #\Newline))
                     (incf index)))))
               ((export-gfm-ascii-punctuation-p character)
                (write-char #\\ output)
                (write-char character output))
               (t (write-char character output)))
             (incf index))))

(defun export-gfm-escape-html-attribute (text)
  (with-output-to-string (output)
    (loop for character across text
          do (case character
               (#\& (write-string "&amp;" output))
               (#\< (write-string "&lt;" output))
               (#\> (write-string "&gt;" output))
               (#\" (write-string "&quot;" output))
               (t (write-char character output))))))

(defun export-gfm-longest-run (text character)
  (loop with longest = 0
        with current = 0
        for item across text
        do (if (char= item character)
               (setf longest (max longest (incf current)))
               (setf current 0))
        finally (return longest)))

(defun export-gfm-code-span (value)
  (let* ((text (substitute #\Space #\Newline
                           (export-gfm-normalize-newlines value)))
         (delimiter (make-string (1+ (export-gfm-longest-run text #\`))
                                 :initial-element #\`))
         (pad-p (and (plusp (length text))
                     (or (find (char text 0) '(#\Space #\Tab #\`))
                         (find (char text (1- (length text)))
                               '(#\Space #\Tab #\`))))))
    (if pad-p
        (format nil "~A ~A ~A" delimiter text delimiter)
        (format nil "~A~A~A" delimiter text delimiter))))

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
  (let* ((body (export-gfm-normalize-newlines value))
         (fence (export-gfm-code-fence body)))
    (with-output-to-string (output)
      (write-string fence output)
      (when language (write-string language output))
      (write-char #\Newline output)
      (write-string body output)
      (unless (and (plusp (length body))
                   (char= (char body (1- (length body))) #\Newline))
        (write-char #\Newline output))
      (write-string fence output))))

(defun export-gfm-slug (title)
  (let ((slug
          (with-output-to-string (output)
            (loop with pending-separator = nil
                  with wrote-character = nil
                  for character across title
                  do (if (alphanumericp character)
                         (progn
                           (when (and pending-separator
                                      wrote-character)
                             (write-char #\- output))
                           (setf pending-separator nil)
                           (write-char (char-downcase character) output)
                           (setf wrote-character t))
                         (setf pending-separator t))))))
    (concatenate 'string "org-" (if (zerop (length slug)) "section" slug))))

(defun export-gfm-unique-id (state candidate)
  (let ((id candidate)
        (suffix 1))
    (loop while (gethash (string-downcase id)
                         (export-gfm-state-used-ids state))
          do (incf suffix)
             (setf id (format nil "~A-~D" candidate suffix)))
    (setf (gethash (string-downcase id)
                   (export-gfm-state-used-ids state))
          t)
    id))

(defun export-gfm-register-reference (state name id)
  (when (and (stringp name) (plusp (length name)))
    (setf (gethash (string-downcase name)
                   (export-gfm-state-reference-ids state))
          id)))

(defun export-gfm-heading-reference (state node)
  (or (gethash node (export-gfm-state-heading-ids state))
      (let ((custom-id (node-property node :custom-id)))
        (export-gfm-unique-id
         state (if (and (stringp custom-id) (plusp (length custom-id)))
                   custom-id
                   (export-gfm-slug (or (node-property node :raw-value) "")))))))

(defun export-gfm-link-target-id (state node)
  (let* ((type (node-property node :type))
         (path (node-property node :path)))
    (when (and (stringp type) (stringp path))
      (cond
        ((string-equal type "custom-id")
         (or (gethash (string-downcase path)
                      (export-gfm-state-reference-ids state))
             path))
        ((member type '("fuzzy" "radio") :test #'string-equal)
         (gethash (string-downcase path)
                  (export-gfm-state-reference-ids state)))))))

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
                    (if (and (stringp custom-id) (plusp (length custom-id)))
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
  (with-output-to-string (output)
    (loop with first-p = t
          for string in strings
          unless (zerop (length string))
            do (unless first-p (write-string separator output))
               (write-string string output)
               (setf first-p nil))))

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

(defun export-gfm-render-views (state views &key preserve-source-breaks-p)
  (with-output-to-string (output)
    (let ((previous-view nil)
          (first-p t))
      (dolist (view views)
        (let ((rendered
                (export-gfm-render-view
                 state view
                 :preserve-source-breaks-p preserve-source-breaks-p)))
          (unless (zerop (length rendered))
            (unless first-p
              (write-string
               (make-string (+ 2 (export-gfm-view-trailing-post-blank
                                  previous-view))
                            :initial-element #\Newline)
               output))
            (write-string rendered output)
            (setf previous-view view
                  first-p nil)))))))

(defun export-gfm-render-inline-views
    (state views &key table-cell-p preserve-source-breaks-p)
  (with-output-to-string (output)
    (dolist (view views)
      (write-string (export-gfm-render-inline-view
                     state view :table-cell-p table-cell-p
                     :preserve-source-breaks-p preserve-source-breaks-p)
                    output))))

(defun export-gfm-inline-code-value (node)
  (let* ((type (node-type node))
         (value (or (node-property node :value) (node-text node) ""))
         (delimiter (case type
                      (:code #\~)
                      (:verbatim #\=)))
         (end (- (length value) (node-post-blank node))))
    (if (and delimiter
             (> end 1)
             (char= (char value 0) delimiter)
             (char= (char value (1- end)) delimiter))
        (subseq value 1 (1- end))
        value)))

(defun export-gfm-literal-view-text (view)
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view)))
    (cond
      ((member type '(:code :verbatim :inline-src-block) :test #'eq)
       (export-gfm-inline-code-value node))
      ((eq type :entity)
       (or (node-property node :utf-8) (node-property node :unicode)
           (node-property node :html) (node-text node) ""))
      ((eq type :link)
       (or (node-property node :path) ""))
      (children
       (with-output-to-string (output)
         (dolist (child children)
           (write-string (export-gfm-literal-view-text child) output))))
      (t (or (node-text node) "")))))

(defun export-gfm-link-label (view &key table-cell-p)
  (let* ((node (export-view-node view))
         (children (export-view-children view))
         (text (if children
                   (with-output-to-string (output)
                     (dolist (child children)
                       (write-string (export-gfm-literal-view-text child)
                                     output)))
                   (or (node-property node :path) ""))))
    (export-gfm-escape-literal text :table-cell-p table-cell-p
                               :soft-newline-p (not table-cell-p))))

(defun export-gfm-serialize-destination (destination)
  (with-output-to-string (output)
    (write-char #\< output)
    (loop for character across destination
          do (case character
               (#\Space (write-string "%20" output))
               (#\< (write-string "%3C" output))
               (#\> (write-string "%3E" output))
               ;; Preserve literal URI ampersands after Markdown entity decoding.
               (#\& (write-string "&amp;" output))
               (t (write-char character output))))
    (write-char #\> output)))

;; GFM expands character references before browsers interpret the destination.
(defun export-gfm-scheme-obfuscation-p (destination)
  (let ((entity (position #\& destination))
        (boundary (position-if (lambda (character)
                                 (find character "/?#"))
                               destination)))
    (and entity
         (null (export-explicit-url-scheme destination))
         (or (null boundary) (< entity boundary)))))

(defun export-gfm-link-source-control-p (node)
  (let ((source (node-text node)))
    (and source (notany #'export-control-character-p source))))

(defun export-gfm-raw-bracket-link-path (node)
  (let ((source (node-text node)))
    (when (and (stringp source)
               (>= (length source) 4)
               (string= "[[" source :end2 2))
      (let ((close (link-object-bracket-close source 2 (length source))))
        (when close
          (link-object-normalize-bracket-path
           (link-object-unescape-path (subseq source 2 close))))))))

(defun export-gfm-expanded-bracket-path (node raw-path)
  (let ((document (node-document node)))
    (if document
        (link-object-expand-abbreviation document raw-path)
        raw-path)))

(defun export-gfm-link-destination (state node)
  (when (export-gfm-link-source-control-p node)
    (let* ((type (node-property node :type))
           (path (node-property node :path))
           (format (node-property node :format))
           (raw-path (and (eq format :bracket)
                          (export-gfm-raw-bracket-link-path node)))
           (raw-destination (or raw-path path))
           (expanded-path (and raw-path
                               (export-gfm-expanded-bracket-path node raw-path)))
           (target-id (export-gfm-link-target-id state node))
           (destination
             (cond
               ((and target-id
                     (stringp type)
                     (member type '("custom-id" "fuzzy" "radio")
                             :test #'string-equal))
                (concatenate 'string "#" target-id))
               ((eq format :bracket) expanded-path)
               ((and (stringp type) (stringp path))
                (concatenate 'string type ":" path))
               ((stringp path) path))))
      (when (and (stringp raw-destination)
                 (stringp destination)
                 (not (export-gfm-scheme-obfuscation-p raw-destination))
                 (not (export-gfm-scheme-obfuscation-p destination))
                 (export-safe-url-p raw-destination destination))
        (export-gfm-serialize-destination destination)))))

(defun export-gfm-image-path-p (path)
  (and (stringp path)
       (let ((extension (position #\. path :from-end t)))
         (and extension
              (member (string-downcase (subseq path (1+ extension)))
                      '("png" "jpg" "jpeg" "gif" "svg" "webp")
                      :test #'string=)))))

(defun export-gfm-render-link (state view &key table-cell-p)
  (let* ((node (export-view-node view))
         (destination (export-gfm-link-destination state node))
         (image-p (export-gfm-image-path-p (node-property node :path)))
         (label (export-gfm-link-label view :table-cell-p table-cell-p)))
    (if destination
        (format nil "~A[~A](~A)" (if image-p "!" "") label destination)
        label)))

(defun export-gfm-render-inline-view
    (state view &key table-cell-p preserve-source-breaks-p)
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view))
         (content (lambda ()
                    (export-gfm-render-inline-views
                     state children :table-cell-p table-cell-p
                     :preserve-source-breaks-p preserve-source-breaks-p)))
         (rendered
           (case type
      ((:plain-text :text)
       (export-gfm-escape-literal (or (node-text node) "")
                                  :table-cell-p table-cell-p
                                  :soft-newline-p
                                  (not (or table-cell-p
                                           preserve-source-breaks-p))
                                  :trim-terminal-newline-p
                                  preserve-source-breaks-p))
      (:bold (format nil "**~A**" (funcall content)))
      (:italic (format nil "*~A*" (funcall content)))
      (:strike-through
       (concatenate 'string "~~" (funcall content) "~~"))
      (:underline (format nil "<u>~A</u>" (funcall content)))
      (:subscript (format nil "<sub>~A</sub>" (funcall content)))
      (:superscript (format nil "<sup>~A</sup>" (funcall content)))
      ((:code :verbatim :inline-src-block)
       (export-gfm-code-span (export-gfm-inline-code-value node)))
      (:entity
       (export-gfm-escape-literal
        (or (node-property node :utf-8) (node-property node :unicode)
            (node-property node :html) (node-text node) "")
        :table-cell-p table-cell-p))
      (:link (export-gfm-render-link state view :table-cell-p table-cell-p))
      (:export-snippet (export-gfm-render-snippet state node))
      (:line-break (if table-cell-p "<br>" (format nil "  ~%")))
      (:footnote-reference (export-gfm-render-footnote-reference state view))
      ((:target :radio-target)
       (export-gfm-render-target state view))
      ((:babel-call :inline-babel-call :citation :citation-reference
        :latex-fragment :statistics-cookie :timestamp)
       (export-gfm-escape-literal (or (node-text node) "")
                                  :table-cell-p table-cell-p
                                  :soft-newline-p
                                  (not (or table-cell-p
                                           preserve-source-breaks-p))))
      (:macro (funcall content))
      (otherwise
       (if children
           (funcall content)
           (export-gfm-escape-literal (or (node-text node) "")
                                      :table-cell-p table-cell-p
                                      :soft-newline-p
                                      (not (or table-cell-p
                                               preserve-source-breaks-p))))))))
    (if (plusp (node-post-blank node))
        (concatenate 'string rendered
                     (make-string (node-post-blank node)
                                  :initial-element #\Space))
        rendered)))

(defun export-gfm-render-heading (state view)
  (let* ((node (export-view-node view))
         (level (or (node-property node :level) 1))
         (title (or (node-property node :raw-value) ""))
         (tags (node-property node :tags))
         (id (export-gfm-heading-reference state node))
         (heading
           (with-output-to-string (output)
             (when (gethash node
                            (export-gfm-state-anchored-headings state))
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
               (write-char #\: output))))
         (children (export-gfm-render-views state (export-view-children view))))
    (if (zerop (length children))
        heading
        (format nil "~A~%~%~A" heading children))))

(defun export-gfm-toc (state)
  (let ((entries (export-preparation-toc
                  (export-gfm-state-preparation state))))
    (export-gfm-join
     (mapcar (lambda (entry)
               (let* ((view (getf entry :view))
                      (node (export-view-node view))
                      (level (getf entry :level))
                      (id (export-gfm-heading-reference state node))
                      (title (or (getf entry :title) "")))
                 (format nil "~A- [~A](~A)"
                         (make-string (* 2 (1- level)) :initial-element #\Space)
                         (export-gfm-escape-literal title)
                         (export-gfm-serialize-destination
                          (concatenate 'string "#" id)))))
             entries)
     (string #\Newline))))

(defun export-gfm-render-quote (state view)
  (let ((text (export-gfm-render-views state (export-view-children view))))
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
  (let* ((children (export-view-children view))
         (paragraph (find-if
                     (lambda (child)
                       (eq (node-type (export-view-node child)) :paragraph))
                     children))
         (prefix (format nil "~A~A "
                         (make-string (* 2 depth) :initial-element #\Space)
                         marker))
         (first-text
           (if paragraph
               (export-gfm-render-inline-views
                state (export-view-children paragraph))
               (if children
                   (export-gfm-render-view state (first children))
                   "")))
         (checkbox (node-property (export-view-node view) :checkbox))
         (checkbox-text (case checkbox
                          (:off "[ ] ")
                          (:on "[X] ")
                          (:trans "[-] ")))
         (remaining (if paragraph (remove paragraph children :test #'eq)
                        (rest children))))
    (with-output-to-string (output)
      (write-string prefix output)
      (when checkbox-text (write-string checkbox-text output))
      (write-string first-text output)
      (dolist (child remaining)
        (let ((rendered (export-gfm-render-view state child)))
          (unless (zerop (length rendered))
            (write-char #\Newline output)
            (loop with start = 0
                  for newline = (position #\Newline rendered :start start)
                  do (write-string
                      (make-string (* 2 (1+ depth)) :initial-element #\Space)
                      output)
                     (write-string rendered output :start start :end newline)
                     (when newline
                       (write-char #\Newline output)
                       (setf start (1+ newline)))
                  while newline)))))))

(defun export-gfm-render-list (state view)
  (let* ((node (export-view-node view))
         (kind (node-property node :type))
         (ordered-p (eq kind :ordered)))
    (export-gfm-join
     (loop for item in (export-view-children view)
           for number from 1
           for item-node = (export-view-node item)
           for marker = (if ordered-p
                            (format nil "~A." (or (node-property item-node :counter)
                                                  number))
                            (or (node-property item-node :bullet) "-"))
           collect (export-gfm-render-list-item state item marker 0))
     (string #\Newline))))

(defun export-gfm-table-cell (state view)
  (let ((children (export-view-children view)))
    (if children
        (export-gfm-render-inline-views state children :table-cell-p t)
        (export-gfm-escape-literal
         (or (node-property (export-view-node view) :value)
             (node-text (export-view-node view)) "")
         :table-cell-p t))))

(defun export-gfm-table-row (state view column-count)
  (let ((cells (export-view-children view)))
    (with-output-to-string (output)
      (write-char #\| output)
      (loop for index below column-count
            for cell = (nth index cells)
            do (write-char #\Space output)
               (when cell (write-string (export-gfm-table-cell state cell) output))
               (write-string " |" output)))))

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
          (write-string (export-gfm-table-row state header column-count) output)
          (write-char #\Newline output)
          (write-char #\| output)
          (loop repeat column-count
                do (write-string " --- |" output))
          (dolist (row body)
            (write-char #\Newline output)
            (write-string (export-gfm-table-row state row column-count) output))))))

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
               (reference-id (format nil "fnr.~A~A"
                                     label
                                     (if (= count 1) ""
                                         (format nil ".~D" (1- count))))))
          (setf (gethash label
                         (export-gfm-state-footnote-reference-counts state))
                count)
          (format nil
                  "<sup><a id=\"~A\" class=\"footref\" href=\"#fn.~A\" role=\"doc-backlink\">~D</a></sup>"
                  (export-gfm-escape-html-attribute reference-id)
                  (export-gfm-escape-html-attribute label)
                  number)))
      ((eq (node-property node :type) :inline)
       (export-gfm-render-inline-views state (export-view-children view)))
      (t (export-gfm-escape-literal (or label ""))))))

(defun export-gfm-render-footnote-definition (state entry)
  (let* ((number (getf entry :number))
         (view (getf entry :definition))
         (children (export-view-children view))
         (content
           (export-gfm-join
            (mapcar (lambda (child)
                      (if (eq (node-type (export-view-node child)) :paragraph)
                          (export-gfm-render-inline-views
                           state (export-view-children child))
                          (export-gfm-render-view state child)))
                    children)
            (format nil "~%~%"))))
    (format nil
            "<sup><a id=\"fn.~A\" class=\"footnum\" href=\"#fnr.~A\">~D</a></sup> ~A"
            number number number content)))

(defun export-gfm-render-footnotes (state)
  (let ((entries (export-preparation-footnotes
                  (export-gfm-state-preparation state))))
    (when entries
      (format nil "## Footnotes~%~%~A"
              (export-gfm-join
               (mapcar (lambda (entry)
                         (export-gfm-render-footnote-definition state entry))
                       entries)
               (format nil "~%~%"))))))

(defun export-gfm-render-target (state view)
  (let* ((node (export-view-node view))
         (value (node-property node :value))
         (id (or (gethash (and (stringp value) (string-downcase value))
                          (export-gfm-state-reference-ids state))
                 (export-gfm-slug (or value "")))))
    (if (eq (node-type node) :radio-target)
        (format nil "<a id=\"~A\">~A</a>"
                (export-gfm-escape-html-attribute id)
                (export-gfm-escape-literal (or value "")))
        (format nil "<a id=\"~A\"></a>"
                (export-gfm-escape-html-attribute id)))))

(defun export-gfm-render-src-block (state view)
  (let* ((node (export-view-node view))
         (mode (getf (export-view-payload view) :exports))
         (value (or (node-property node :value) ""))
         (code (export-gfm-fenced-block value
                                        (export-gfm-source-language node)))
         (results (export-gfm-render-views state (export-view-children view))))
    (case mode
      (:none "")
      (:results results)
      (:code code)
      (otherwise (if (zerop (length results)) code
                     (format nil "~A~%~%~A" code results))))))

(defun export-gfm-render-export-block (state node)
  (let ((backend (node-property node :type))
        (value (or (node-property node :value) "")))
    (if (and (stringp backend) (string-equal backend "gfm"))
        (if (export-preparation-allow-raw-html
             (export-gfm-state-preparation state))
            value
            (export-gfm-fenced-block value))
        "")))

(defun export-gfm-render-snippet (state node)
  (let ((backend (node-property node :back-end))
        (value (or (node-property node :value) "")))
    (if (and (stringp backend) (string-equal backend "gfm"))
        (if (export-preparation-allow-raw-html
             (export-gfm-state-preparation state))
            value
            (export-gfm-escape-literal value))
        "")))

(defun export-gfm-render-view (state view &key preserve-source-breaks-p)
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
      ((:table-row :table-cell)
       (export-gfm-render-inline-views state children :table-cell-p t))
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
                       state children :preserve-source-breaks-p t)))
         (with-output-to-string (output)
           (loop with start = 0
                 for newline = (position #\Newline content :start start)
                 do (write-string content output :start start :end newline)
                    (when newline
                      (write-string "  " output)
                      (write-char #\Newline output)
                      (setf start (1+ newline)))
                 while newline))))
      (:footnote-definition "")
      ((:comment :comment-block :keyword :property-drawer :node-property
        :planning :clock)
       "")
      ((:bold :italic :strike-through :underline :subscript :superscript
        :code :verbatim :inline-src-block :entity :link :export-snippet
        :line-break
        :footnote-reference :target :radio-target :babel-call
        :inline-babel-call :citation :citation-reference :latex-fragment
        :statistics-cookie :timestamp :plain-text :text)
       (export-gfm-render-inline-view
        state view :preserve-source-breaks-p preserve-source-breaks-p))
      (:latex-environment
       (export-gfm-escape-literal (or (node-text node) "")))
      (otherwise (export-gfm-escape-literal (or (node-text node) ""))))))

(defun export-gfm-render-document (state)
  (let* ((preparation (export-gfm-state-preparation state))
         (toc (export-gfm-toc state))
         (body (export-gfm-render-views
                state (export-preparation-root-views preparation)))
         (footnotes (export-gfm-render-footnotes state)))
    (export-gfm-join
     (remove-if-not (lambda (text)
                      (and (stringp text) (plusp (length text))))
                    (list toc body footnotes))
     (format nil "~%~%"))))

(defmethod export-render-preparation
    ((preparation export-preparation) (backend (eql :gfm)))
  (let ((state (%make-export-gfm-state :preparation preparation)))
    (export-gfm-index-state state)
    (export-gfm-render-document state)))
